program HexToBinEmu;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  CPUEmulator8086 in 'CPUEmulator8086.pas',
  CPUInstructionSet8086 in 'CPUInstructionSet8086.pas',
  BIOSEmulator8086 in 'BIOSEmulator8086.pas',
  OSEmulator8086 in 'OSEmulator8086.pas';

const                                         //Address  Instruction
  HexToBinProgram: TBytes = [$BB, $F0, $F0,   //100      MOV      BX,F0F0       ; The value to display as binary
                             $B1, $10,        //103      MOV      CL,10         ; The number of times to loop, 10 hex = 16 decimal = 16 bits
                             $D1, $D3,        //105      RCL      BX,1          ; Rotate Carry Left
                             $B2, $30,        //107      MOV      DL,30         ; Set DL to character '0'
                             $80, $D2, $00,   //109      ADC      DL,00         ; Add the carry flag to DL, this results in '0' or '1'
                             $B4, $02,        //10C      MOV      AH,02         ; Select INT 21 function 02: output character in DL
                             $CD, $21,        //10E      INT      21            ; BIOS Interrupt INT 21
                             $E2, $F3,        //110      LOOPW    0105          ; Go back to address 105 and loop until CL = 0 | $F3 = -13 bytes relative offset
                             $CD, $20];       //112      INT      20            ; Terminate program

var
  CPU: TCPU8086CPU;
  Mem: TCPU8086Mem;
  OS: TCPU8086OS;
  BIOS: TCPU8086BIOS;
begin
  try
    Writeln('Booting 8086 emulator');

    CPU.Reset;
    Mem.Reset;

    OS := TCPU8086MooooOS.Create(@Mem);

    BIOS := TCPU8086CowInputOutputSystem.Create(@Mem, OS);

    try
      OS.LoadProgram(HexToBinProgram);

      {$IFDEF DEBUG}
      Mem.DumpToFile('c:\temp\8086.mem');
      {$ENDIF}

      while (not OS.ProgramTerminated) do
        ExecuteInstruction(@CPU, @Mem, [BIOS, OS]);

      Writeln('');
      Writeln('');
      Writeln('Program terminated');
    finally
      FreeAndNil(BIOS);

      FreeAndNil(OS);
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
