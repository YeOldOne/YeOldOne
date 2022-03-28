unit OSEmulator8086;

interface

uses
  System.Sysutils, CPUEmulator8086;

// https://wiki.osdev.org/Memory_Map_(x86)
// Interrupt Vector Table is located at 0000:0000
//
// https://sw0rdm4n.wordpress.com/category/interrupt-vector-table/#:~:text=Interrupt%20vector%20table%20on%208086,dos%20interrupt%20service%20routine%20address%2C
// OS interrupts are 20 - FF

type
  TCPU8086OS = class(TCPU8086EmulationCodeInterception)
  private const
    IVT_SEGMENT                    = $0000;
    OS_BOOT_SECTOR_SEGMENT         = $007C;
    OS_BOOT_SECTOR_POINTER         = $0000;
    CONVENTIONAL_MEMORY_SEGEMENT_1 = $0050;                                     //Map OS functions here (RMA) $00500 - $07BFF
    CONVENTIONAL_MEMORY_SEGEMENT_2 = $07E0;                                     //Program space         (RMA) $07E00 - $7FFFF
  public const
    EMU_OS_INTERRUPT_SEGMENT = CONVENTIONAL_MEMORY_SEGEMENT_1;
  strict private
    FMemory: PCPU8086Mem;
    FProgramTerminated: Boolean;
    FBootSector: array[$0000..$01FF] of Byte;                                   //512 byte in-memory boot sector
    procedure WriteOSBootSector;
    procedure WriteOSInterruptVectorTable;
    procedure EmulatedInterrupt(const CPU: PCPU8086CPU; const Mem: PCPU8086Mem; const Interrupt: Byte);
    function GetBootSector: PByte;
  strict protected
    function OSInfo: string; virtual; abstract;
  public
    constructor Create(const Memory: PCPU8086Mem);
    procedure LoadProgram(const ProgramData: TBytes);
    //Intercept custom emulation calls
    function UsedByEmu(const CPU: PCPU8086CPU; const Mem: PCPU8086Mem; const Segment, Offset: TCPU8086Register): Boolean; override;
    property BootSector: PByte read GetBootSector;                              //512 bytes boot sector
    property ProgramTerminated: Boolean read FProgramTerminated;
  end;

  TCPU8086MooooOS = class(TCPU8086OS)
  strict private
  strict protected
    function OSInfo: string; override;
  public
  end;

implementation

{******************************************************************************}

constructor TCPU8086OS.Create(const Memory: PCPU8086Mem);
begin
  FMemory := Memory;

  WriteOSBootSector;

  WriteOSInterruptVectorTable;
end;

procedure TCPU8086OS.WriteOSBootSector;
var
  OSString: AnsiString;
begin
  //Zero out boot sector
  FillChar(FBootSector[0], Length(FBootSector), 0);

  //Set data segment to code segment
  FBootSector[$00] := $0E;          //0000 PUSH       CS                 ; push code segment
  FBootSector[$01] := $1F;          //0001 POP        DS                 ; pop data segment

  //Set AH=0 DX=addr Int 21, write string to output
  FBootSector[$02] := $B4;          //0002 MOV        AH, 09
  FBootSector[$03] := $09;
  FBootSector[$04] := $BA;          //0004 MOV        DX, 013            ; at offset 0013 the string starts
  FBootSector[$05] := $13;          //                                   ;  offset low
  FBootSector[$06] := $00;          //                                   ;  offset high
  FBootSector[$07] := $CD;          //0007 INT        21
  FBootSector[$08] := $21;

  //Set data segment to program entry code segment
  FBootSector[$09] := $B8;          //0009 MOV        AX, CONVENTIONAL_MEMORY_SEGEMENT_2
  FBootSector[$0A] := Lo(CONVENTIONAL_MEMORY_SEGEMENT_2);
  FBootSector[$0B] := Hi(CONVENTIONAL_MEMORY_SEGEMENT_2);

  FBootSector[$0C] := $50;          //000C PUSH       AX
  FBootSector[$0D] := $1F;          //000D POP        DS

  //Jump to program entry at CONVENTIONAL_MEMORY_SEGEMENT_2:0000, but should start at offset 0100, so (CONVENTIONAL_MEMORY_SEGEMENT_2 - 10):0100
  FBootSector[$0E] := $EA;          //000E JMP        (CONVENTIONAL_MEMORY_SEGEMENT_2 - 10):0100
  FBootSector[$0F] := $00;
  FBootSector[$10] := $01;
  FBootSector[$11] := Lo(CONVENTIONAL_MEMORY_SEGEMENT_2 - $10);
  FBootSector[$12] := Hi(CONVENTIONAL_MEMORY_SEGEMENT_2 - $10);

  //String to display goes here at 0013
  OSString := AnsiString(OSInfo + #13#10'$');

  Move(OSString[1], FBootSector[$13], Length(OSString));
end;

function TCPU8086OS.GetBootSector: PByte;
begin
  //For a Disk Operating System this would be the Master Boot Record (MBR) which is stored at the first sector of the hard drive

  Result := @FBootSector[0];
end;

procedure TCPU8086OS.WriteOSInterruptVectorTable;
var
  BaseOfIVT: PWord;

  procedure WriteOSInterrupt(const InterruptNr: Byte);
  var
    IntTable: PWord;
  begin
    IntTable := PWord(NativeUInt(BaseOfIVT) + (InterruptNr * 2 * SizeOf(Word))); //Two word values per entry

    //Memory pointer
    IntTable^ := InterruptNr;                                                   //Just store the interrupt number as the pointer/offset

    //Memory segment
    Inc(IntTable);
    IntTable^ := EMU_OS_INTERRUPT_SEGMENT;                                      //Constant value to identify OS interrupt calls
  end;

var
  InterruptNumber: Integer;
begin
  BaseOfIVT := PWord(FMemory^.Address[IVT_SEGMENT, $0000]);

  //Write IVT entries for OS interrupts $20..$FF
  for InterruptNumber := $20 to $FF do
    WriteOSInterrupt(InterruptNumber);
end;

function TCPU8086OS.UsedByEmu(const CPU: PCPU8086CPU; const Mem: PCPU8086Mem; const Segment, Offset: TCPU8086Register): Boolean;
var
  CallMem: NativeUInt;
  InterruptCallMem: NativeUInt;
  InterruptNumber: NativeUInt;
begin
  Result := False;

  CallMem := NativeUInt(FMemory^.Address[Segment, Offset]) - NativeUInt(FMemory);

  InterruptCallMem := NativeUInt(FMemory^.Address[EMU_OS_INTERRUPT_SEGMENT, 0]) - NativeUInt(FMemory);

  //Is call within the emulated interrupt calls?
  if (CallMem >= InterruptCallMem) and (CallMem <= InterruptCallMem + $FF) then
  begin
    InterruptNumber := CallMem - InterruptCallMem;

    //Only accept OS interrupts
    case (InterruptNumber) of
      $20..$FF:
        begin
          EmulatedInterrupt(CPU, Mem, InterruptNumber);

          Result := True
        end;
    else
      Exit;
    end;
  end;
end;

procedure TCPU8086OS.LoadProgram(const ProgramData: TBytes);
var
  EntryPoint: PWord;
begin
  EntryPoint := FMemory^.Address[CONVENTIONAL_MEMORY_SEGEMENT_2, $0000];

  Move(ProgramData[0], EntryPoint^, Length(ProgramData));
end;

procedure TCPU8086OS.EmulatedInterrupt(const CPU: PCPU8086CPU; const Mem: PCPU8086Mem; const Interrupt: Byte);
var
  Text: PAnsiChar;
begin
  case (Interrupt) of
    $20: FProgramTerminated := True;
    $21: case (CPU^.A.H) of
           $02: begin
                  //Write character to standard output
                  //DL = character code
                  //Return AL = DL

                  //http://www.gabrielececchetti.it/Teaching/CalcolatoriElettronici/Docs/i8086_and_DOS_interrupts.pdf
                  //Page 9 of 19

                  //Instead of writing the character in DL to the BIOS video mem, which a 'real' 8086 would have done
                  // just output the character to the console, this is the benefit of having an OS interrupt handler outside the emulator address space
                  Write(Char(CPU^.D.L));

                  //Set return value
                  CPU^.A.L := CPU^.D.L;
                end;
           $09: begin
                  //Write string to standard output
                  //DS:DX = '$' terminated string
                  //Return AL = 0x24

                  //http://spike.scu.edu.au/~barry/interrupts.html#ah09

                  //Instead of writing the string to the BIOS video mem, which a 'real' 8086 would have done
                  // just output the string to the console, this is the benefit of having an OS interrupt handler outside the emulator address space

                  Text := PAnsiChar(Mem^.Address[CPU^.DS, CPU^.D.X]);           //DS:DX

                  //Linebreaks are CRLF = #13#10
                  //Too simple string parser, but suits the current purpose
                  while (Text^ <> '$') do
                  begin
                    if (Text^ = #13) then                                       //Use #13 as CRLF
                      Writeln('')
                    else if (Text^ <> #10) then                                 //Ignore #10
                      Write(Char(Text^));

                    Inc(Text);
                  end;

                  //Set return value
                  CPU^.A.L := $24;
                end;
         else
           raise Exception.Create('Function 0x' + IntToHex(CPU^.A.H, 2) + ' of interrupt 0x' + IntToHex(Interrupt, 2) + ' not yet supported');
         end;
  else
    raise Exception.Create('OS interrupt 0x' + IntToHex(Interrupt, 2) + ' not yet supported');
  end;
end;

{******************************************************************************}

function TCPU8086MooooOS.OSInfo: string;
begin
  Result := 'MooooOS! 1.0 ready';
end;

{******************************************************************************}

end.
