unit GB.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, GBGpu, GBJoypad,
  FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.Effects, GBRom, GBMemory, GBCpu,
  GBTimer, GBSound, GBMbc;

type
  TFormMain = class(TForm)
    PaintBoxScreen: TPaintBox;
    TimerFPS: TTimer;
    LabelFPS: TLabel;
    RectangleBG: TRectangle;
    LayoutBottom: TLayout;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    LayoutClient: TLayout;
    LayoutScreen: TLayout;
    InnerGlowEffect: TInnerGlowEffect;
    LayoutShadow: TLayout;
    ShadowEffect: TShadowEffect;
    LayoutScreenLayer: TLayout;
    RectangleScreenBlack: TRectangle;
    RectangleScreenFrame: TRectangle;
    Rectangle3: TRectangle;
    Layout1: TLayout;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LayoutOverlay: TLayout;
    Layout2: TLayout;
    Rectangle4: TRectangle;
    Rectangle5: TRectangle;
    Circle1: TCircle;
    Layout3: TLayout;
    Circle2: TCircle;
    Circle3: TCircle;
    Layout4: TLayout;
    Label4: TLabel;
    Label5: TLabel;
    Layout5: TLayout;
    Rectangle6: TRectangle;
    Label6: TLabel;
    Rectangle7: TRectangle;
    Label7: TLabel;
    RectangleLoad: TRectangle;
    Label8: TLabel;
    Layout6: TLayout;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    OpenDialogROM: TOpenDialog;
    RectangleOFF: TRectangle;
    Label17: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxScreenPaint(Sender: TObject; Canvas: TCanvas);
    procedure TimerFPSTimer(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure LayoutClientMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormDestroy(Sender: TObject);
    procedure RectangleOFFClick(Sender: TObject);
  private
    FFPS, FFPSTotal: Integer;
    FWork: Boolean;
    FBuffer: TBitmap;
    FGBROM: TGBRom;
    FGBMem: TGBMemory;
    FGBCPU: TGBCpu;
    FGBGPU: TGBGpu;
    FGBSound: TGBSound;
    FGBMBC: TGBMbc;
    FGBJoyPad: TGBJoypad;
    procedure LoadROM(const FileName: string);
  public
    procedure DrawScreen(const ScreenData: TScreenArray);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.ButtonLoadClick(Sender: TObject);
begin
  if OpenDialogROM.Execute then
    LoadROM(OpenDialogROM.FileName);
end;

procedure TFormMain.LoadROM(const FileName: string);
var
  getStream: TFileStream;
begin
  if Assigned(FGBCPU) then
  begin
    if not FGBCPU.Paused then
    begin
      FGBCPU.Paused := True;
      LoadROM(FileName);
      Exit;
    end;
  end;
  FGBROM := TGBRom.Create;
  getStream := TFileStream.Create(FileName, fmOpenRead or fmShareExclusive);
  try
    getStream.Position := 0;
    FGBROM.ReadROM(getStream);
  finally
    getStream.Free;
  end;
  FGBGPU := TGBGpu.Create(DrawScreen);
  FGBMBC := TGBMbc.Create(@FGBROM);
  FGBMem := TGBMemory.Create(@FGBMBC, @FGBGPU);
  FGBSound := TGBSound.Create(@FGBMem);
  FGBCPU := TGBCpu.Create(@FGBMem, @FGBGPU, @FGBSound);
  FGBCPU.SkipBios;
  //  cpu.main;
  //  ShowMessage('done');

  // UP=87,DOWN=83,LEFT=65,RIGHT=68
  TGBJoypad.Instance.GBKeys[TGBKey.UP] := 119;
  TGBJoypad.Instance.GBKeys[TGBKey.DOWN] := 115;
  TGBJoypad.Instance.GBKeys[TGBKey.LEFT] := 97;
  TGBJoypad.Instance.GBKeys[TGBKey.RIGHT] := 100;
    // A=74,B=75,SELECT=90,START=88
  TGBJoypad.Instance.GBKeys[TGBKey.A] := 106;
  TGBJoypad.Instance.GBKeys[TGBKey.B] := 107;
  TGBJoypad.Instance.GBKeys[TGBKey.SELECT] := 99;
  TGBJoypad.Instance.GBKeys[TGBKey.START] := 120;
  {
  w = 119
  a = 97
  s = 115
  d = 100
  x = 120
  c = 99
  j = 106
  k = 107   }

  FWork := True;
  while (not FGBCPU.Paused) and (not Application.Terminated) do
    FGBCPU.step(1);

  FWork := False;
  FreeAndNil(FGBROM);
  FreeAndNil(FGBMem);
  FreeAndNil(FGBGPU);
  FreeAndNil(FGBSound);
  FreeAndNil(FGBMBC);
  FreeAndNil(FGBCPU);
end;

procedure TFormMain.DrawScreen(const ScreenData: TScreenArray);
var
  Data: TBitmapData;
begin
  Inc(FFPSTotal);
  if not TimerFPS.Enabled then
    TimerFPS.Enabled := True;
  //Label1.Text := IntToStr(_fpsTotal);
  if FBuffer.Map(TMapAccess.Write, Data) then
  try
    with Data do
    begin
      for var y := 0 to 144 - 1 do
      begin
        var Line: PRGB32Array := GetScanline(y);
        for var x := 0 to 160 - 1 do
        begin
          case ScreenData[160 * y + x] of
            0:
              begin//255Ј¬255Ј¬255
                Line[x].B := 208;
                Line[x].G := 248;
                Line[x].R := 224;
                Line[x].A := 255;
              end;
            1:
              begin//192Ј¬192Ј¬192
                Line[x].B := 112;
                Line[x].G := 192;
                Line[x].R := 136;
                Line[x].A := 255;
              end;
            2:
              begin//96Ј¬96Ј¬96
                Line[x].B := 86;
                Line[x].G := 104;
                Line[x].R := 52;
                Line[x].A := 255;
              end;
            3:
              begin//40,40,40
                Line[x].B := 32;
                Line[x].G := 24;
                Line[x].R := 8;
                Line[x].A := 255;
              end;
          end;
        end;
      end;
    end;
  finally
    FBuffer.Unmap(Data);
  end;
  PaintBoxScreen.Repaint;
  Application.ProcessMessages;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  InnerGlowEffect.Enabled := True;
  ShadowEffect.Enabled := True;
  FFPS := 0;
  FFPSTotal := 0;
  FBuffer := TBitmap.Create;
  FBuffer.Width := 160;
  FBuffer.Height := 144;
  FWork := False;

  FGBROM := nil;
  FGBMem := nil;
  FGBCPU := nil;
  FGBGPU := nil;
  FGBSound := nil;
  FGBMBC := nil;
 // KeyPreview := True;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  //Memo1.Lines.Add(KeyChar + ' = ' + Ord(KeyChar).ToString);
  FGBJoyPad.Instance.GBKeyDown(Ord(KeyChar));
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  FGBJoyPad.Instance.GBKeyUp(Ord(KeyChar));
end;

procedure TFormMain.LayoutClientMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  StartWindowDrag;
end;

procedure TFormMain.PaintBoxScreenPaint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.BeginScene(nil);
  try
    Canvas.DrawBitmap(FBuffer, TRectF.Create(0, 0, FBuffer.Width, FBuffer.Height), PaintBoxScreen.LocalRect, 1, True);
  finally
    Canvas.EndScene;
  end;
end;

procedure TFormMain.RectangleOFFClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFormMain.TimerFPSTimer(Sender: TObject);
begin
  Inc(FFPS);
  LabelFPS.Text := IntToStr(FFPSTotal div FFPS);
end;

end.

