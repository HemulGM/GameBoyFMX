program GameBoyEmuFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  GB.Main in 'GB.Main.pas' {FormMain},
  GBCartridge in 'Core\GBCartridge.pas',
  GBCpu in 'Core\GBCpu.pas',
  GBGpu in 'Core\GBGpu.pas',
  GBInterruptManager in 'Core\GBInterruptManager.pas',
  GBJoypad in 'Core\GBJoypad.pas',
  GBMbc in 'Core\GBMbc.pas',
  GBMemory in 'Core\GBMemory.pas',
  GBRom in 'Core\GBRom.pas',
  GBSound in 'Core\GBSound.pas',
  GBSoundChannel in 'Core\GBSoundChannel.pas',
  GBTimer in 'Core\GBTimer.pas',
  GBTypes in 'Core\GBTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
