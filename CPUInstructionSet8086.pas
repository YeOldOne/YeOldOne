unit CPUInstructionSet8086;

interface

uses
  System.SysUtils, CPUEmulator8086;

// http://aturing.umcs.maine.edu/~meadow/courses/cos335/8086-instformat.pdf
//
// 8086 instruction template
// A general template for most of the 8086 instructions
//
//       Bit                       Bit                       Bit
// Byte  7 6 5 4 3 2 1 0           7 6 5 4 3 2 1 0           7 6 5 4 3 2 1 0
//  1   | opcode    |d|w|         | opcode        |          | opcode
//  2   |mod| reg | r/m |                                      long         |
//  3   | [optional]    |
//  4   | [optional]    |
//  5   | [optional]    |
//  6   | [optional]    |
//
// [1] d = direction (or s = sign extension) ( or c = count)
// [2] w = word/byte
// [3] mod = mode
// [4] reg = register/segment register
// [5] r/m = register/memory
//
// [1]
//   direction
//     1          data moves from operand specified by R/M field to operand specified by REG field
//     0          data moves from operand specified by REG field to operand specified by R/M field
//
//   sign extension
//     1          one byte of immediate data is present which muct be sign-extended to produce a 16-bit operand
//     0          two bytes of immediate are present
//
//   count (for shift and rotate instructions)
//     1          CL is used for shift count
//     0          CL is not used for shift count
//
// [2]
//   word/byte
//     1          data is word
//     0          data is byte
//
// [3]
//   mode
//     00         use R/M table 1 for R/M operand
//     01         use R/M table 2 with 8-bit displacement
//     10         use R/M table 2 with 16-bit displacement
//     11         two register instruction; use register table [4]
//
// [4]
//   register     w=0   w=1
//     000        AL    AX
//     001        CL    CX
//     010        DL    DX
//     011        BL    BX
//     100        AH    SP
//     101        CH    BP
//     110        DH    SI
//     111        BH    DI
//
//   segment register
//     000        ES
//     001        CS
//     010        SS
//     110        DS
//
// [5]
//   register/memory
//
//   table 1
//     000        [BX+SI]
//     001        [BX+DI]
//     010        [BP+SI]
//     011        [BP+DI]
//     100        [SI]
//     101        [DI]
//     110        Direct address
//     111        [BX]
//
//   table 2
//     000        [BX+SI]
//     001        [BX+DI]
//     010        [BP+SI]
//     011        [BP+DI]
//     100        [SI]
//     101        [DI]
//     110        [BP]
//     111        [BX]

// All ASM Manual pages refer to: http://www.nj7p.info/Manuals/PDFs/Intel/121703-003.pdf

type
  TCPU8086Instruction = packed record
  public type
    TMode = (mRMTable1, mRMTable2w8bit, mRMTable2w16bit, mRegTable);
    TRegister = (rALAX, rCLCX, rDLDX, rBLBX, rAHSP, rCHBP, rDHSI, rBHDI);
    TSegmentRegister = (rES, rCS, rSS, rDS);
    TRegisterMemoryTable1 = (rmt1BXSI, rmt1BXDI, rmt1BPSI, rmt1BPDI, rmt1Srmt1I, rmt1DI, rmt1DirectAddress, rmt1BX);
    TRegisterMemoryTable2 = (rmt2BXSI, rmt2BXDI, rmt2BPSI, rmt2BPDI, rmt2SI, rmt2DI, rmt2BP, rmt2BX);
  strict private
    FInstruction: Byte;
    function GetOpCode: Byte;
    function GetDirection: Boolean;
    function GetWordByte: Boolean;
    function GetMode: TMode;
    function GetReg: TRegister;
    function GetSegReg: TSegmentRegister;
    function GetRegMem1: TRegisterMemoryTable1;
    function GetRegMem2: TRegisterMemoryTable2;
  private
    function GetByte(const Index: Integer): Byte;
    function GetWord(const Index: Integer): Word;
  public
    //Operation Code, 6 bit
    property OpCode: Byte read GetOpCode;
    //Instruction details
    property Direction: Boolean read GetDirection;
    property WordByte: Boolean read GetWordByte;
    property Mode: TMode read GetMode;
    property Reg: TRegister read GetReg;
    property SegReg: TSegmentRegister read GetSegReg;
    property RegMem1: TRegisterMemoryTable1 read GetRegMem1;
    property RegMem2: TRegisterMemoryTable2 read GetRegMem2;
    property OpCode1: Byte index 0 read GetByte;
    property OpCode2: Byte index 1 read GetByte;
    property Optional1: Byte index 2 read GetByte;
    property Optional2: Byte index 3 read GetByte;
    property Optional3: Byte index 4 read GetByte;
    property Optional4: Byte index 5 read GetByte;
  end;

  //TCPU8086Instruction isn't meant to be used a data structure
  //Instead use the memory address of the instruction as PCPU8086Instruction to access the instruction data
  PCPU8086Instruction = ^TCPU8086Instruction;

procedure ExecuteInstruction(const CPU: PCPU8086CPU; const Mem: PCPU8086Mem; const Interceptors: array of TCPU8086EmulationCodeInterception);

implementation

{******************************************************************************}

function TCPU8086Instruction.GetByte(const Index: Integer): Byte;
begin
  Result := PByte(NativeUInt(@FInstruction) + NativeUInt(SizeOf(Byte) * Index))^;
end;

function TCPU8086Instruction.GetWord(const Index: Integer): Word;
begin
  Result := PWord(NativeUInt(@FInstruction) + NativeUInt(SizeOf(Byte) * Index))^;
end;

function TCPU8086Instruction.GetOpCode: Byte;
begin
  //First 6 bits of byte 1
  // yet don't shift right, clear bytes 0 and 1 instead
  Result := FInstruction and not $03;                                           //$03 is mask 0000 0011
end;

function TCPU8086Instruction.GetDirection: Boolean;
begin
  //Bit 1 of byte 1
  Result := (FInstruction and $02) <> 0;
end;

function TCPU8086Instruction.GetWordByte: Boolean;
begin
  //Bit 0 of byte 1
  Result := (FInstruction and $01) <> 0;
end;

function TCPU8086Instruction.GetMode: TMode;
begin
  //First 2 bits of byte 1
  Result := TMode((GetByte(1) shr 6) and $03);                                  //$03 is mask 0000 0011
end;

function TCPU8086Instruction.GetReg: TRegister;
begin
  //Bit 5, 4 and 3 of byte 1
  Result := TRegister((GetByte(1) shr 3) and $07);                              //$07 is mask 0000 0111
end;

function TCPU8086Instruction.GetSegReg: TSegmentRegister;
begin
  //Bit 5, 4 and 3 of byte 1
  Result := TSegmentRegister((GetByte(1) shr 3) and $07);                       //$07 is mask 0000 0111
end;

function TCPU8086Instruction.GetRegMem1: TRegisterMemoryTable1;
begin
  //Last 3 bits of byte 1
  Result := TRegisterMemoryTable1(GetByte(1) and $07);                          //$07 is mask 0000 0111
end;

function TCPU8086Instruction.GetRegMem2: TRegisterMemoryTable2;
begin
  //Last 3 bits of byte 1
  Result := TRegisterMemoryTable2(GetByte(1) and $07);                          //$07 is mask 0000 0111
end;

{******************************************************************************}

procedure ExecuteInstruction(const CPU: PCPU8086CPU; const Mem: PCPU8086Mem; const Interceptors: array of TCPU8086EmulationCodeInterception);
const
  IVT_SEGMENT = $0000;
var
  Instruction: PCPU8086Instruction;

  function CallIsIntercepted(const Segment, Offset: TCPU8086Register): Boolean;
  var
    Counter: Integer;
  begin
    Result := False;

    for Counter := Low(Interceptors) to High(Interceptors) do
      if (Interceptors[Counter].UsedByEmu(CPU, Mem, Segment, Offset)) then
        Exit(True);
  end;

  procedure PushWord(const Value: TCPU8086Register);
  var
    Stack: PWord;
  begin
    //Increase stack pointer
    CPU^.SP := CPU^.SP + SizeOf(TCPU8086Register);

    //Get stack address
    Stack := PWord(Mem^.Address[CPU^.SS, CPU^.SP]);                             //stack segment:stack pointer

    //Set stack value
    Stack^ := Value;
  end;

  function PopWord: TCPU8086Register;
  var
    Stack: PWord;
  begin
    //Get stack address
    Stack := PWord(Mem^.Address[CPU^.SS, CPU^.SP]);                             //stack segment:stack pointer

    //Read value from stack address
    Result := Stack^;

    //Decrease stack pointer
    CPU^.SP := CPU^.SP - SizeOf(TCPU8086Register);
  end;

  procedure Push_CS;
  //8086 ASM Manual: 6-73
  begin
    PushWord(CPU^.CS);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 1;
  end;

  procedure Pop_DS;
  //8086 ASM Manual: 6-73
  begin
    CPU^.DS := PopWord;

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 1;
  end;

  procedure Push_AX;
  //8086 ASM Manual: 6-73
  begin
    PushWord(CPU^.A.X);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 1;
  end;

  procedure Push_CX;
  //8086 ASM Manual: 6-73
  begin
    PushWord(CPU^.C.X);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 1;
  end;

  procedure Push_DX;
  //8086 ASM Manual: 6-73
  begin
    PushWord(CPU^.D.X);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 1;
  end;

  procedure Push_BX;
  //8086 ASM Manual: 6-73
  begin
    PushWord(CPU^.B.X);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 1;
  end;

  procedure Push_SP;
  //8086 ASM Manual: 6-73
  begin
    PushWord(CPU^.SP);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 1;
  end;

  procedure Push_BP;
  //8086 ASM Manual: 6-73
  begin
    PushWord(CPU^.BP);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 1;
  end;

  procedure Push_SI;
  //8086 ASM Manual: 6-73
  begin
    PushWord(CPU^.SI);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 1;
  end;

  procedure Push_DI;
  //8086 ASM Manual: 6-73
  begin
    PushWord(CPU^.DI);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 1;
  end;

  procedure Adc_8;
  //8086 ASM Manual: 6-25
  var
    Mode: Byte;
    Value: Byte;
    RegisterTable: Byte;
    AddedValue: Word;
  begin
    Mode := Instruction^.GetByte(1) shr 6;

    //Mode table [3]
    case (Mode) of
      $00: begin
             //use R/M table 1 for R/M operand

             raise Exception.Create('ADC_8 mode 0 not yet supported');
           end;
      $01: begin
             //use R/M table 2 with 8-bit displacement

             raise Exception.Create('ADC_8 mode 1 not yet supported');
           end;
      $02: begin
             //use R/M table 2 with 16-bit displacement

             raise Exception.Create('ADC_8 mode 2 not yet supported');
           end;
      $03: begin
             //two register instruction; use register table [4]

             RegisterTable := Instruction^.GetByte(1) and $07;

             case (RegisterTable) of
               $00: Value := CPU^.A.L;
               $01: Value := CPU^.C.L;
               $02: Value := CPU^.D.L;
               $03: Value := CPU^.B.L;
               $04: Value := CPU^.A.H;
               $05: Value := CPU^.C.H;
               $06: Value := CPU^.D.H;
               $07: Value := CPU^.B.H;
             else
               raise Exception.Create('Invalid ADC_8 register table index');
             end;

             AddedValue := Value + Instruction^.GetByte(2);

             if (CPU^.Flags.CY) then
               AddedValue := AddedValue + 1;

             case (RegisterTable) of
               $00: CPU^.A.L := AddedValue and $00FF;
               $01: CPU^.C.L := AddedValue and $00FF;
               $02: CPU^.D.L := AddedValue and $00FF;
               $03: CPU^.B.L := AddedValue and $00FF;
               $04: CPU^.A.H := AddedValue and $00FF;
               $05: CPU^.C.H := AddedValue and $00FF;
               $06: CPU^.D.H := AddedValue and $00FF;
               $07: CPU^.B.H := AddedValue and $00FF;
             else
               raise Exception.Create('Invalid ADC_8 register table index');
             end;

             CPU^.Flags.O := AddedValue > $FF;                                  //Overflow
             CPU^.Flags.S := False;                                             //Sign
             CPU^.Flags.Z := AddedValue = $00;                                  //Zero
             CPU^.Flags.AC := (AddedValue and $0100) <> 0;                      //Auxiliary Carry
             CPU^.Flags.P := (AddedValue and $0001) <> 0;                       //Parity
             CPU^.Flags.CY := (AddedValue and $0100) <> 0;                      //Carry

             //Progress the instruction pointer
             CPU^.IP := CPU^.IP + 3;
           end;
    else
      raise Exception.Create('Invalid ADC_8 mode');
    end;
  end;

  procedure Call_far;
  //8086 ASM Manual: 6-30
  type
    TCallFar = packed record
      OpCode: Byte;
      Offset: Word;
      Segment: Word;
    end;
  var
    CallFar: TCallFar;
  begin
    Move(Instruction^, CallFar, SizeOf(TCallFar));

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + SizeOf(TCallFar);

    //Handle intercepted calls by the emulator
    if (CallIsIntercepted(CallFar.Segment, CallFar.Offset)) then
      Exit;

    //Push return instruction pointer to the stack
    PushWord(CPU^.IP);

    //Push return code segment to the stack
    PushWord(CPU^.CS);

    //Set code segment:instruction pointer to the call address
    CPU^.CS := CallFar.Segment;
    CPU^.IP := CallFar.Offset;
  end;

  procedure Mov_AL_val;
  //8086 ASM Manual: 6-62
  begin
    CPU^.A.L := Instruction^.GetByte(1);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 2;
  end;

  procedure Mov_CL_val;
  //8086 ASM Manual: 6-62
  begin
    CPU^.C.L := Instruction^.GetByte(1);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 2;
  end;

  procedure Mov_DL_val;
  //8086 ASM Manual: 6-62
  begin
    CPU^.D.L := Instruction^.GetByte(1);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 2;
  end;

  procedure Mov_BL_val;
  //8086 ASM Manual: 6-62
  begin
    CPU^.B.L := Instruction^.GetByte(1);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 2;
  end;

  procedure Mov_AH_val;
  //8086 ASM Manual: 6-62
  begin
    CPU^.A.H := Instruction^.GetByte(1);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 2;
  end;

  procedure Mov_CH_val;
  //8086 ASM Manual: 6-62
  begin
    CPU^.C.H := Instruction^.GetByte(1);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 2;
  end;

  procedure Mov_DH_val;
  //8086 ASM Manual: 6-62
  begin
    CPU^.D.H := Instruction^.GetByte(1);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 2;
  end;

  procedure Mov_BH_val;
  //8086 ASM Manual: 6-62
  begin
    CPU^.B.H := Instruction^.GetByte(1);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 2;
  end;

  procedure Mov_AX_val;
  //8086 ASM Manual: 6-62
  begin
    CPU^.A.X := Instruction^.GetWord(1);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 3;
  end;

  procedure Mov_CX_val;
  //8086 ASM Manual: 6-62
  begin
    CPU^.C.X := Instruction^.GetWord(1);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 3;
  end;

  procedure Mov_DX_val;
  //8086 ASM Manual: 6-62
  begin
    CPU^.D.X := Instruction^.GetWord(1);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 3;
  end;

  procedure Mov_BX_val;
  //8086 ASM Manual: 6-62
  begin
    CPU^.B.X := Instruction^.GetWord(1);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 3;
  end;

  procedure Mov_SP_val;
  //8086 ASM Manual: 6-62
  begin
    CPU^.SP := Instruction^.GetWord(1);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 3;
  end;

  procedure Mov_BP_val;
  //8086 ASM Manual: 6-62
  begin
    CPU^.BP := Instruction^.GetWord(1);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 3;
  end;

  procedure Mov_SI_val;
  //8086 ASM Manual: 6-62
  begin
    CPU^.SI := Instruction^.GetWord(1);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 3;
  end;

  procedure Mov_DI_val;
  //8086 ASM Manual: 6-62
  begin
    CPU^.DI := Instruction^.GetWord(1);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 3;
  end;

  procedure Int;
  //8086 ASM Manual: 6-50
  var
    InterruptNumber: Byte;
    BaseOfIVT: PWord;
    CurrentInterrupt: PWord;
    Offset: TCPU8086Register;
    Segment: TCPU8086Register;
  begin
    InterruptNumber := Instruction^.GetByte(1);

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 2;

    //Get the base of the interrupt vector table
    BaseOfIVT := Mem^.Address[IVT_SEGMENT, $0000];

    //Jump to the table entry of the current interrupt
    CurrentInterrupt := PWord(NativeUInt(BaseOfIVT) + (InterruptNumber * 2 * SizeOf(Word))); //Two word values per entry

    //Get call offset
    Offset := CurrentInterrupt^;

    //Get call segement
    Inc(CurrentInterrupt);
    Segment := CurrentInterrupt^;

    //Handle intercepted calls by the emulator
    if (CallIsIntercepted(Segment, Offset)) then
      Exit;

    //Push return instruction pointer to the stack
    PushWord(CPU^.IP);

    //Push return code segment to the stack
    PushWord(CPU^.CS);

    //Set code segment:instruction pointer to the call address
    CPU^.CS := Segment;
    CPU^.IP := Offset;
  end;

  procedure Rcl;
  //8086 ASM Manual: 6-77
  var
    Mode: Byte;
    Value: Word;
    CarryFlag: Boolean;
    RegisterTable: Byte;
  begin
    Mode := Instruction^.GetByte(1) shr 6;

    //Mode table [3]
    case (Mode) of
      $00: begin
             //use R/M table 1 for R/M operand

             raise Exception.Create('RCL mode 0 not yet supported');
           end;
      $01: begin
             //use R/M table 2 with 8-bit displacement

             raise Exception.Create('RCL mode 1 not yet supported');
           end;
      $02: begin
             //use R/M table 2 with 16-bit displacement

             raise Exception.Create('RCL mode 2 not yet supported');
           end;
      $03: begin
             //two register instruction; use register table [4]

             RegisterTable := Instruction^.GetByte(1) and $07;

             case (RegisterTable) of
               $00: Value := CPU^.A.X;
               $01: Value := CPU^.C.X;
               $02: Value := CPU^.D.X;
               $03: Value := CPU^.B.X;
               $04: Value := CPU^.SP;
               $05: Value := CPU^.BP;
               $06: Value := CPU^.SI;
               $07: Value := CPU^.DI;
             else
               raise Exception.Create('Invalid RCL register table index');
             end;

             CarryFlag := (Value and $8000) <> 0;

             Value := Value shl 1;

             if (CarryFlag) then
               Value := Value or $0001;

             //Progress the instruction pointer
             CPU^.IP := CPU^.IP + 2;

             case (RegisterTable) of
               $00: CPU^.A.X := Value;
               $01: CPU^.C.X := Value;
               $02: CPU^.D.X := Value;
               $03: CPU^.B.X := Value;
               $04: CPU^.SP := Value;
               $05: CPU^.BP := Value;
               $06: CPU^.SI := Value;
               $07: CPU^.DI := Value;
             else
               raise Exception.Create('Invalid RCL register table index');
             end;

             CPU^.Flags.CY := CarryFlag;
           end;
    else
      raise Exception.Create('Invalid RCL mode');
    end;
  end;

  procedure LoopCXNot0;
  //8086 ASM Manual: 6-61
  var
    Offset: ShortInt;
  begin
    Offset := ShortInt(Instruction^.GetByte(1));

    //Progress the instruction pointer
    CPU^.IP := CPU^.IP + 2;

    //Decrement CX
    CPU^.C.X := CPU^.C.X - 1;

    //Done when CX = 0
    if (CPU^.C.X = 0) then
      Exit;

    //Jump
    CPU^.IP := CPU^.IP + Offset;
  end;

  procedure Jump_far;
  //8086 ASM Manual: 6-55
  type
    TJumpFar = packed record
      OpCode: Byte;
      Offset: Word;
      Segment: Word;
    end;
  var
    JumpFar: TJumpFar;
  begin
    Move(Instruction^, JumpFar, SizeOf(TJumpFar));

    //Set code segment:instruction pointer to the new address
    CPU^.CS := JumpFar.Segment;
    CPU^.IP := JumpFar.Offset;
  end;

begin
  Instruction := PCPU8086Instruction(Mem^.Address[CPU^.CS, CPU^.IP]);           //code segment:instruction pointer

  case (Instruction^.OpCode1) of
    $0E: Push_CS;
    $1F: Pop_DS;
    $50: Push_AX;
    $51: Push_CX;
    $52: Push_DX;
    $53: Push_BX;
    $54: Push_SP;
    $55: Push_BP;
    $56: Push_SI;
    $57: Push_DI;
    $80: Adc_8;
    $9A: Call_far;
    $B0: Mov_AL_val;
    $B1: Mov_CL_val;
    $B2: Mov_DL_val;
    $B3: Mov_BL_val;
    $B4: Mov_AH_val;
    $B5: Mov_CH_val;
    $B6: Mov_DH_val;
    $B7: Mov_BH_val;
    $B8: Mov_AX_val;
    $B9: Mov_CX_val;
    $BA: Mov_DX_val;
    $BB: Mov_BX_val;
    $BC: Mov_SP_val;
    $BD: Mov_BP_val;
    $BE: Mov_SI_val;
    $BF: Mov_DI_val;
    $CD: Int;
    $D1: Rcl;
    $E2: LoopCXNot0;
    $EA: Jump_far;
  end;

end;

{******************************************************************************}

end.
