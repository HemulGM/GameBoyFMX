unit GB.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, GBGpu, GBJoypad,
  FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.Effects;

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
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxScreenPaint(Sender: TObject; Canvas: TCanvas);
    procedure TimerFPSTimer(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure LayoutClientMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  private
    _fps, _fpsTotal: Integer;
    _bmp: TBitmap;
  public
    _gbJoyPad: TGBJoypad;
    procedure drawScreen(const pscreen: PScreenArr);
  end;

var
  FormMain: TFormMain;

implementation

uses
  GBRom, GBMemory, GBCpu, GBTimer, GBSound, GBMbc;

{$R *.fmx}

procedure TFormMain.ButtonLoadClick(Sender: TObject);
var
  getStream: TFileStream;
  getPath: string;
  rom: TGBRom;
  mem: TGBMemory;
  cpu: TGBCpu;
  gpu: TGBGpu;
  pgpu: PGBGpu;
  sound: TGBSound;
  psound: PGBSound;
  pmem: PGBMemory;
  _gbrom: PGBRom;
  _mbc: TGBMbc;
  _Pmbc: PGBMbc;
begin
  getPath := 'D:\Projects\#Fork\DelphiGBEmu\roms\Super Mario Land (JUE) (V1.1) [!].gb';
  getPath := 'D:\Projects\GameBoyFMX\roms\Legend of Zelda, The - Link''s Awakening (U) (V1.2) [!].gb';
  getPath := 'D:\Projects\GameBoyFMX\roms\Mortal Kombat 3 (U) [!].gb';
  getStream := TFileStream.Create(getPath, fmOpenRead or fmShareExclusive);
  getStream.Position := 0;
  rom := TGBRom.Create;
  rom.readRom(getStream);
  getStream.Free;
  gpu := TGBGpu.Create(drawScreen);
  pgpu := @gpu;
  _gbrom := @rom;
  _mbc := TGBMbc.Create(_gbrom);
  _Pmbc := @_mbc;
  mem := TGBMemory.Create(_Pmbc, pgpu);
  pmem := @mem;
  sound := TGBSound.Create(pmem);
  psound := @sound;
  cpu := TGBCpu.Create(pmem, pgpu, psound);
//  Application.ProcessMessages;
  cpu.skipBios;
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

  while (not cpu.Paused) and (not Application.Terminated) do
    cpu.step(1);
end;

procedure TFormMain.drawScreen(const pscreen: PScreenArr);
var
  x, y: Integer;
  Line: PRGB32Array;
  Data: TBitmapData;
begin
  Inc(_fpsTotal);
  if not TimerFPS.Enabled then
  begin
    TimerFPS.Enabled := True;
  end;
  //Label1.Text := IntToStr(_fpsTotal);
  if _bmp.Map(TMapAccess.Write, Data) then
  try
    with Data do
    begin
      for y := 0 to 144 - 1 do
      begin
        Line := GetScanline(y);
        for x := 0 to 160 - 1 do
        begin
          case pscreen^[160 * y + x] of
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
    _bmp.Unmap(Data);
  end;
//  Image1.Invalidate;
  //Image1.Picture.Bitmap := _bmp;
  PaintBoxScreen.Repaint;
  Application.ProcessMessages;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  InnerGlowEffect.Enabled := True;
  ShadowEffect.Enabled := True;
  _fps := 0;
  _fpsTotal := 0;
  _bmp := TBitmap.Create;
  _bmp.Width := 160;
  _bmp.Height := 144;
 // KeyPreview := True;
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  //Memo1.Lines.Add(KeyChar + ' = ' + Ord(KeyChar).ToString);
  _gbJoyPad.Instance.GBKeyDown(Ord(AnsiChar(KeyChar)));
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  _gbJoyPad.Instance.GBKeyUp(Ord(AnsiChar(KeyChar)));
end;

procedure TFormMain.LayoutClientMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  StartWindowDrag;
end;

procedure TFormMain.PaintBoxScreenPaint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.BeginScene(nil);
  try
    Canvas.DrawBitmap(_bmp, TRectF.Create(0, 0, _bmp.Width, _bmp.Height), PaintBoxScreen.LocalRect, 1, True);
  finally
    Canvas.EndScene;
  end;
end;

procedure TFormMain.TimerFPSTimer(Sender: TObject);
begin
  Inc(_fps);
  LabelFPS.Text := IntToStr(_fpsTotal div _fps);
end;

end.

