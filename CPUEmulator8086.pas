unit CPUEmulator8086;

interface

uses
  System.SysUtils, System.Classes;

type
  TCPU8086Register = Word;

  //https://en.wikipedia.org/wiki/Intel_8086#:~:text=The%208086%20has%20eight%20more,%2C%20are%2016%2Dbit%20only.
  TCPU8086MainRegister = packed record
  strict private
    FX: TCPU8086Register;
    procedure SetH(const Value: Byte);
    procedure SetL(const Value: Byte);
    procedure SetX(const Value: Word);
    function GetH: Byte;
    function GetL: Byte;
  public
    property X: TCPU8086Register read FX write SetX;                            //16 bit value
    property H: Byte read GetH write SetH;                                      //High 8 bit value
    property L: Byte read GetL write SetL;                                      //Low 8 bit value
  end;

  //https://www.tutorialspoint.com/flag-register-of-8086-microprocessor
  //https://www.geeksforgeeks.org/flag-register-8086-microprocessor/
  TCPU8086StatusRegister = packed record
  strict private
    FFlags: TCPU8086Register;
    procedure SetFlag(const Index: Integer; const Value: Boolean);
    function GetFlag(const Index: Integer): Boolean;
  public
    //Flag register
    property Flags: TCPU8086Register read FFlags write FFlags;
    //Status flags
    property CY: Boolean index  0 read GetFlag write SetFlag;                   //Carry
    property P : Boolean index  2 read GetFlag write SetFlag;                   //Parity
    property AC: Boolean index  4 read GetFlag write SetFlag;                   //Auxiliary Carry
    property Z : Boolean index  6 read GetFlag write SetFlag;                   //Zero
    property S : Boolean index  7 read GetFlag write SetFlag;                   //Sign
    property O : Boolean index 11 read GetFlag write SetFlag;                   //Overflow
    //Control flags
    property T : Boolean index  8 read GetFlag write SetFlag;                   //Trap, causes "interrupt 1" after next instruction
    property I : Boolean index  9 read GetFlag write SetFlag;                   //Interrupt, true=enabled
    property D : Boolean index 10 read GetFlag write SetFlag;                   //Directional, in string ops
  end;

  //https://en.wikipedia.org/wiki/Intel_8086#:~:text=The%208086%20has%20eight%20more,%2C%20are%2016%2Dbit%20only.
  TCPU8086CPU = packed record
  strict private
    //Main registers
    FA: TCPU8086MainRegister;
    FB: TCPU8086MainRegister;
    FC: TCPU8086MainRegister;
    FD: TCPU8086MainRegister;
    //Index registers
    FSI: TCPU8086Register;
    FDI: TCPU8086Register;
    FBP: TCPU8086Register;
    FSP: TCPU8086Register;
    //Program counter
    FIP: TCPU8086Register;
    //Segment registers
    FCS: TCPU8086Register;
    FDS: TCPU8086Register;
    FES: TCPU8086Register;
    FSS: TCPU8086Register;
    //Status register
    FFlags: TCPU8086StatusRegister;
  public
    procedure Reset;
    //Data group
    property A: TCPU8086MainRegister read FA write FA;                          //Primary accumulator
    property B: TCPU8086MainRegister read FB write FB;                          //Base, accumulator
    property C: TCPU8086MainRegister read FC write FC;                          //Counter, accumulator
    property D: TCPU8086MainRegister read FD write FD;                          //Data, accumulator
    //Pointer and index group
    property SI: TCPU8086Register read FSI write FSI;                           //Source Index
    property DI: TCPU8086Register read FDI write FDI;                           //Destination Index
    property BP: TCPU8086Register read FBP write FBP;                           //Base Pointer
    property SP: TCPU8086Register read FSP write FSP;                           //Stack Pointer
    //Program counter
    property IP: TCPU8086Register read FIP write FIP;                           //Instruction Pointer
    //Segment register set
    property CS: TCPU8086Register read FCS write FCS;                           //Code segment
    property DS: TCPU8086Register read FDS write FDS;                           //Data segment
    property ES: TCPU8086Register read FES write FES;                           //Extra segment
    property SS: TCPU8086Register read FSS write FSS;                           //Stack Segment
    //Status register
    property Flags: TCPU8086StatusRegister read FFlags write FFlags;            //Flags
  end;

  PCPU8086CPU = ^TCPU8086CPU;

  TCPU8086Mem = packed record
  strict private
    FMem: array[$0..$FFFFF] of Byte;                                            //1 MiB, addressable in 20 bits
    function GetAddress(const Segment, Offset: TCPU8086Register): PWord;
  public
    procedure Reset;
    {$IFDEF DEBUG}
    procedure DumpToFile(const FileName: string);
    {$ENDIF}
    property Address[const Segment, Offset: TCPU8086Register]: PWord read GetAddress;
  end;

  PCPU8086Mem = ^TCPU8086Mem;

  //Base class to intercept emulated CPU jumps into emulation objects that run outside of the emulation
  TCPU8086EmulationCodeInterception = class(TObject)
  public
    function UsedByEmu(const CPU: PCPU8086CPU; const Mem: PCPU8086Mem; const Segment, Offset: TCPU8086Register): Boolean; virtual; abstract;
  end;

implementation

{******************************************************************************}

procedure TCPU8086MainRegister.SetH(const Value: Byte);
begin
  FX := (FX and $00FF) or (Value shl 8);
end;

procedure TCPU8086MainRegister.SetL(const Value: Byte);
begin
  FX := (FX and $FF00) or Value;
end;

procedure TCPU8086MainRegister.SetX(const Value: Word);
begin
  FX := Value;
end;

function TCPU8086MainRegister.GetH: Byte;
begin
  Result := FX shr 8;
end;

function TCPU8086MainRegister.GetL: Byte;
begin
  Result := FX and $FF;
end;

{******************************************************************************}

procedure TCPU8086StatusRegister.SetFlag(const Index: Integer; const Value: Boolean);
var
  Mask: TCPU8086Register;
begin
  Mask := $FFFF xor (1 shl Index);

  //Clear flag bit
  Flags := Flags and Mask;

  //Set flag bit
  if (Value) then
    Flags := Flags or (1 shl Index);
end;

function TCPU8086StatusRegister.GetFlag(const Index: Integer): Boolean;
begin
  Result := (FFlags and (1 shl Index)) <> 0;
end;

{******************************************************************************}

procedure TCPU8086CPU.Reset;
begin
  //Zero out all registers
  FillChar(Self, SizeOf(TCPU8086CPU), 0);

  //Interrupts are enabled
  //8086 ASM Manual: 6-17
  //http://www.nj7p.info/Manuals/PDFs/Intel/121703-003.pdf
  Flags.I := True;

  //CPU starts up at FFFF:0000
  //https://stackoverflow.com/questions/4004493/what-address-does-the-x86-begin-executing-at
  CS := $FFFF;                                                                  //Code Segment
  IP := $0000;                                                                  //Instruction Pointer
end;

{******************************************************************************}

procedure TCPU8086Mem.Reset;
begin
  //Zero out entire memory
  FillChar(FMem[0], Length(FMem), 0);
end;

function TCPU8086Mem.GetAddress(const Segment, Offset: TCPU8086Register): PWord;
begin
  // Segment 1111 1111 1111 1111 0000     16 bit, left shifted by 4
  //  Offset 0000 1111 1111 1111 1111     16 bit
  // Address ------------------------ +   Add to get the 20 bit address

  Result := PWord( NativeUInt( @FMem[0]     ) +                                 //Base address of allocated emulated memory
                   NativeUInt( Segment shl 4) +                                 //Memory segment
                   NativeUInt( Offset       ) );                                //Memory offset
end;

{$IFDEF DEBUG}
procedure TCPU8086Mem.DumpToFile(const FileName: string);
var
  FileStream: TFileStream;
begin
  if (FileExists(FileName)) then
    DeleteFile(FileName);

  FileStream := TFileStream.Create(FileName, fmCreate);
  try

    FileStream.Write(FMem[0], Length(FMem));

  finally
    FreeAndNil(FileStream);
  end;
end;
{$ENDIF}

{******************************************************************************}

end.
