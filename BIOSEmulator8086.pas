unit BIOSEmulator8086;

interface

// https://wiki.osdev.org/Memory_Map_(x86)
//
// BIOS code is loaded at F000:0000
// BIOS data is located at 0040:0000
// Interrupt Vector Table is located at 0000:0000
//
// https://sw0rdm4n.wordpress.com/category/interrupt-vector-table/#:~:text=Interrupt%20vector%20table%20on%208086,dos%20interrupt%20service%20routine%20address%2C
// BIOS interrupts are 00 - 1F
//
// Instead of programming a BIOS in 8086 code,
// or using an existing BIOS: https://github.com/adriancable/8086tiny
// for 'simplicity' this BIOS exists outside of the 8086 emulated environment
// Calls to the 64k BIOS memory area (F000:0000 - F000:FFFF) can be routed directly to this BIOS emulator
//
// All calls to $FFFE:00XX (EMU_BIOS_INTERRUPT_SEGMENT:interrupt number) and
// all calls to $FFFD:00XX (EMU_BIOS_EXTRA_SEGMENT:function number)
// have to be redirected to this BIOS emulator
//
// Emulated BIOS Interrupt code is located at $FFE0:$0000 - $FFE0:$00FF
//
// Another page reserved for extra emulated BIOS functions is located at $FFD0:$0000 - $FFD0:$00FF

uses
  System.SysUtils, CPUEmulator8086, OSEmulator8086;

type
  TCPU8086BIOS = class(TCPU8086EmulationCodeInterception)
  private const
    IVT_SEGMENT                = $0000;
    BIOS_DATA_SEGMENT          = $0040;
    BIOS_CODE_SEGMENT          = $F000;
    BIOS_ENTRYPOINT_SEGMENT    = $FFFF;
    BIOS_ENTRYPOINT_POINTER    = $0000;
    OS_BOOT_SECTOR_SEGMENT     = $007C;
    OS_BOOT_SECTOR_POINTER     = $0000;
    CALL_OUTPUT_BIOS_INFO      = $00;
  public const
    EMU_BIOS_INTERRUPT_SEGMENT = BIOS_CODE_SEGMENT or $0FE0;
    EMU_BIOS_EXTRA_SEGMENT     = BIOS_CODE_SEGMENT or $0FD0;
  strict private
    FMemory: PCPU8086Mem;
    procedure SetDataByte(const Index: Integer; const Value: Byte);
    procedure SetDataWord(const Index: Integer; const Value: Word);
    procedure WriteBIOSInterruptVectorTable;
    procedure WriteBIOSEntrypoint;
    procedure WriteOSBootSector(const OS: TCPU8086OS);
    procedure EmulatedInterrupt(const CPU: PCPU8086CPU; const Interrupt: Byte);
    procedure ExtraEmulatedCall(const CPU: PCPU8086CPU; const CallNr: Byte);
    function GetDataByte(const Index: Integer): Byte;
    function GetDataWord(const Index: Integer): Word;
    function GetPByte(const Index: Integer): PByte;
  strict protected
    function BIOSInformation: string; virtual; abstract;
  public
    constructor Create(const Memory: PCPU8086Mem; const OS: TCPU8086OS);
    //Intercept custom emulation calls
    function UsedByEmu(const CPU: PCPU8086CPU; const Mem: PCPU8086Mem; const Segment, Offset: TCPU8086Register): Boolean; override;
    //BIOS Data
    property COM1Port: Word index $00 read GetDataWord write SetDataWord;
    property COM2Port: Word index $02 read GetDataWord write SetDataWord;
    property COM3Port: Word index $04 read GetDataWord write SetDataWord;
    property COM4Port: Word index $06 read GetDataWord write SetDataWord;
    property LPT1Port: Word index $08 read GetDataWord write SetDataWord;
    property LPT2Port: Word index $0A read GetDataWord write SetDataWord;
    property LPT3Port: Word index $0C read GetDataWord write SetDataWord;
    property EBDASegment: Word index $0E read GetDataWord write SetDataWord;    //Extended BIOS Data Address (value = address shr 4, so the value actually is the segment), non standardized
    property HardwareFlags: Word index $10 read GetDataWord write SetDataWord;
    property KiBUsedBeforeEBDA: Word index $13 read GetDataWord write SetDataWord;
    property KeyboardState: Word index $17 read GetDataWord write SetDataWord;
    property KeyboardBuffer: PByte index $1E read GetPByte;                     //32 bytes buffer
    property DisplayMode: Byte index $49 read GetDataByte write SetDataByte;
    property TextModeNrColumns: Word index $4A read GetDataWord write SetDataWord;
    property VideoIOPort: Word index $63 read GetDataWord write SetDataWord;
    property IRQTimerTicks: Word index $6C read GetDataWord write SetDataWord;
    property NrHardDrives: Byte index $75 read GetDataByte write SetDataByte;
    property KeyboardBufferStart: Word index $80 read GetDataWord write SetDataWord;
    property KeyboardBufferEnd: Word index $82 read GetDataWord write SetDataWord;
    property LastKeyboardLEDShiftState: Byte index $97 read GetDataByte write SetDataByte;
  end;

  TCPU8086CowInputOutputSystem = class(TCPU8086BIOS)
  strict protected
    function BIOSInformation: string; override;
  end;

implementation

{******************************************************************************}

constructor TCPU8086BIOS.Create(const Memory: PCPU8086Mem; const OS: TCPU8086OS);
begin
  FMemory := Memory;

  KiBUsedBeforeEBDA := ( $400  +                                                //Interrupt table             1 KiB
                         $100  +                                                //BIOS Data                 256 B
                         $FFFF )                                                //BIOS Code                  64 KiB
                       div $400;                                                //Make KiB out of B

  WriteBIOSInterruptVectorTable;

  WriteBIOSEntrypoint;

  WriteOSBootSector(OS);
end;

procedure TCPU8086BIOS.WriteBIOSInterruptVectorTable;
var
  BaseOfIVT: PWord;

  procedure WriteBIOSInterrupt(const InterruptNr: Byte);
  var
    IntTable: PWord;
  begin
    IntTable := PWord(NativeUInt(BaseOfIVT) + (InterruptNr * 2 * SizeOf(Word))); //Two word values per entry

    //Memory pointer
    IntTable^ := InterruptNr;                                                   //Just store the interrupt number as the pointer/offset

    //Memory segment
    Inc(IntTable);
    IntTable^ := EMU_BIOS_INTERRUPT_SEGMENT;                                    //Constant value to identify BIOS interrupt calls
  end;

var
  InterruptNumber: Integer;
begin
  BaseOfIVT := PWord(FMemory^.Address[IVT_SEGMENT, $0000]);

  //Write IVT entries for BIOS interrupts $00..$19
  for InterruptNumber := $00 to $1F do
    WriteBIOSInterrupt(InterruptNumber);
end;

procedure TCPU8086BIOS.WriteBIOSEntrypoint;
const
  JUMP_32BIT_DISPLACEMENT = $EA;
  CALL_32BIT_DISPLACEMENT = $9A;
type
  //8086 ASM Manual: 6-30 and 6-55
  TJump32BitDisplacment = packed record
    OpCode: Byte;
    Offset: Word;
    Segment: Word;
  end;
  PJump32BitDisplacment = ^TJump32BitDisplacment;
var
  EntryPoint: PByte;
  CallToBIOSInfo: PJump32BitDisplacment;
  JumpToOSEntryPoint: PJump32BitDisplacment;
  Bptr: PByte;
begin
  // Initialization code, inside the 8086 emulator, to load the BIOS
  // Since this emulator BIOS doesn't run inside the emulator there's nothing to load/execute
  // (normally this would load the Interrupt Vector Table, BIOS Data, OS boot sector, etc)
  // Now just output BIOS version information through an extra emulated call, then jump to the OS boot sector

  // Note: only 16 bytes available on this location, at the very end of the 1 MiB total memory

  EntryPoint := PByte(FMemory^.Address[BIOS_ENTRYPOINT_SEGMENT, BIOS_ENTRYPOINT_POINTER]);

  //Write the absolute 32 bit call to the emulated extra call: output BIOS info
  CallToBIOSInfo := PJump32BitDisplacment(EntryPoint);

  CallToBIOSInfo^.OpCode := CALL_32BIT_DISPLACEMENT;
  CallToBIOSInfo^.Offset := CALL_OUTPUT_BIOS_INFO;
  CallToBIOSInfo^.Segment := EMU_BIOS_EXTRA_SEGMENT;

  //Write the absolute 32 bit jump to the OS boot sector
  JumpToOSEntryPoint := PJump32BitDisplacment(NativeUInt(EntryPoint) + SizeOf(TJump32BitDisplacment));

  JumpToOSEntryPoint^.OpCode := JUMP_32BIT_DISPLACEMENT;
  JumpToOSEntryPoint^.Offset := OS_BOOT_SECTOR_POINTER;
  JumpToOSEntryPoint^.Segment := OS_BOOT_SECTOR_SEGMENT;

  //10 out of 16 bytes written
  //Fill the remainder with 'Moooo!'
  Bptr := PByte(NativeUInt(EntryPoint) + SizeOf(TJump32BitDisplacment) + SizeOf(TJump32BitDisplacment));
  Bptr^ := Ord('M'); Inc(Bptr);
  Bptr^ := Ord('o'); Inc(Bptr);
  Bptr^ := Ord('o'); Inc(Bptr);
  Bptr^ := Ord('o'); Inc(Bptr);
  Bptr^ := Ord('o'); Inc(Bptr);
  Bptr^ := Ord('!');
end;

procedure TCPU8086BIOS.WriteOSBootSector(const OS: TCPU8086OS);
var
  BootSector: PByte;
begin
  //Load the OS boot sector into memory

  BootSector := PByte(FMemory^.Address[OS_BOOT_SECTOR_SEGMENT, OS_BOOT_SECTOR_POINTER]);

  Move(OS.BootSector^, BootSector^, 512);
end;

procedure TCPU8086BIOS.SetDataByte(const Index: Integer; const Value: Byte);
var
  Address: PByte;
begin
  Address := PByte(FMemory^.Address[BIOS_DATA_SEGMENT, Index]);

  Address^ := Value;
end;

function TCPU8086BIOS.GetDataByte(const Index: Integer): Byte;
var
  Address: PByte;
begin
  Address := PByte(FMemory^.Address[BIOS_DATA_SEGMENT, Index]);

  Result := Address^;
end;

procedure TCPU8086BIOS.SetDataWord(const Index: Integer; const Value: Word);
var
  Address: PWord;
begin
  Address := PWord(FMemory^.Address[BIOS_DATA_SEGMENT, Index]);

  Address^ := Value;
end;

function TCPU8086BIOS.GetDataWord(const Index: Integer): Word;
var
  Address: PWord;
begin
  Address := PWord(FMemory^.Address[BIOS_DATA_SEGMENT, Index]);

  Result := Address^;
end;

function TCPU8086BIOS.GetPByte(const Index: Integer): PByte;
begin
  Result := PByte(FMemory^.Address[BIOS_DATA_SEGMENT, Index]);
end;

function TCPU8086BIOS.UsedByEmu(const CPU: PCPU8086CPU; const Mem: PCPU8086Mem; const Segment, Offset: TCPU8086Register): Boolean;
var
  CallMem: NativeUInt;
  InterruptCallMem: NativeUInt;
  ExtraCallsMem: NativeUInt;
  InterruptNumber: NativeUInt;
  ExtraCallNumber: NativeUInt;
begin
  Result := False;

  CallMem := NativeUInt(FMemory^.Address[Segment, Offset]) - NativeUInt(FMemory);

  InterruptCallMem := NativeUInt(FMemory^.Address[EMU_BIOS_INTERRUPT_SEGMENT, 0]) - NativeUInt(FMemory);

  ExtraCallsMem := NativeUInt(FMemory^.Address[EMU_BIOS_EXTRA_SEGMENT, 0]) - NativeUInt(FMemory);

  //Is call within the emulated interrupt calls?
  if (CallMem >= InterruptCallMem) and (CallMem <= InterruptCallMem + $FF) then
  begin
    InterruptNumber := CallMem - InterruptCallMem;

    //Only accept BIOS interrupts
    case (InterruptNumber) of
      $00..$1F:
        begin
          EmulatedInterrupt(CPU, InterruptNumber);

          Result := True
        end;
    else
      Exit;
    end;
  end;

  //Is call within the emulated extra calls?
  if (CallMem >= ExtraCallsMem) and (CallMem <= ExtraCallsMem + $FF) then
  begin
    ExtraCallNumber := CallMem - ExtraCallsMem;

    ExtraEmulatedCall(CPU, ExtraCallNumber);

    Result := True;
  end;
end;

procedure TCPU8086BIOS.EmulatedInterrupt(const CPU: PCPU8086CPU; const Interrupt: Byte);
begin
  raise Exception.Create('Emulated BIOS interrupts not yet supported');
end;

procedure TCPU8086BIOS.ExtraEmulatedCall(const CPU: PCPU8086CPU; const CallNr: Byte);
begin
  case (CallNr) of
    CALL_OUTPUT_BIOS_INFO: Writeln(BIOSInformation);
  else
    raise Exception.Create('Emulated BIOS call 0x' + IntToHex(CallNr, 2) + ' not yet supported');
  end;
end;

{******************************************************************************}

function TCPU8086CowInputOutputSystem.BIOSInformation: string;
begin
  Result := 'CIOS (Cow Input Output System) version 1.0 loaded at address ' + IntToHex(BIOS_CODE_SEGMENT, 4) + ':' + IntToHex(BIOS_ENTRYPOINT_POINTER, 4);
end;

{******************************************************************************}

end.
