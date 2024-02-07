unit GBRom;

interface

uses
  System.Classes, System.SysUtils;

type
  TCartridgeTypes = record
    id: Integer;
    name: string;
    mbctype: string;
    isRam: Boolean;
    isBat: Boolean;
    isTmr: Boolean;
    isRmbl: Boolean;
    isAccl: Boolean;
  end;

type
  TGBRom = class
  protected
    FTitle: string; // ´æ·ÅromµÄÃû×Ö
    FLocate: string;
    FRAMSize: string;
    FSize: string;
    FCartType: TCartridgeTypes;
    procedure SetTitle;
    procedure SetLocation;
    procedure SetRamSize;
    procedure SetRomSize;
    procedure SetCartridgeType;
  public
    ROMData: TArray<Byte>;
    procedure ReadROM(Stream: TStream);
    procedure ReadBIOS(Stream: TStream);
    function GetTitle: string;
    function GetRamSize: string;
    function GetCartridgeType: TCartridgeTypes;
  end;

type
  PGBRom = ^TGBRom;

implementation

{ TGBRom }

function TGBRom.GetCartridgeType: TCartridgeTypes;
begin
  Result := FCartType;
end;

function TGBRom.GetRamSize: string;
begin
  Result := FRAMSize;
end;

function TGBRom.GetTitle: string;
begin
  Result := FTitle;
end;

procedure TGBRom.ReadBIOS(Stream: TStream);
begin
  if SizeOf(ROMData) > 0 then
  begin

  end;
end;

procedure TGBRom.ReadROM(Stream: TStream);
begin
  Stream.Position := 0;
  SetLength(ROMData, Stream.Size);
  for var i := 0 to Stream.Size - 1 do
    Stream.ReadBuffer(ROMData[i], 1);
  SetTitle;
  SetRamSize;
  SetCartridgeType;
end;

procedure TGBRom.SetCartridgeType;
begin
  case ROMData[$147] of
    $00:
      begin
        with FCartType do
        begin
          id := $00;
          name := 'ROM Only';
          mbctype := 'ROM_ONLY';
        end;
      end;
    $01:
      begin
        with FCartType do
        begin
          id := $01;
          name := 'MBC1';
          mbctype := 'MBC1';
        end;
      end;
    $02:
      begin
        with FCartType do
        begin
          id := $02;
          name := 'MBC1 + RAM';
          mbctype := 'MBC1';
        end;
      end;
    $03:
      begin
        with FCartType do
        begin
          id := $03;
          name := 'MBC1 + RAM + Battery';
          mbctype := 'MBC1';
        end;
      end;
//    $05:begin
//      with cartType do
//      begin
//        id := $00;
//        name := 'ROM Only';
//        mbctype := 'ROM_ONLY';
//      end;
//    end;
//    $06:begin
//      with cartType do
//      begin
//        id := $00;
//        name := 'ROM Only';
//        mbctype := 'ROM_ONLY';
//      end;
//    end;
//    $08:begin
//      with cartType do
//      begin
//        id := $00;
//        name := 'ROM Only';
//        mbctype := 'ROM_ONLY';
//      end;
//    end;
//    $09:begin
//      with cartType do
//      begin
//        id := $00;
//        name := 'ROM Only';
//        mbctype := 'ROM_ONLY';
//      end;
//    end;
//    $0F:begin
//      with cartType do
//      begin
//        id := $00;
//        name := 'ROM Only';
//        mbctype := 'ROM_ONLY';
//      end;
//    end;
//    $10:begin
//      with cartType do
//      begin
//        id := $00;
//        name := 'ROM Only';
//        mbctype := 'ROM_ONLY';
//      end;
//    end;
//    $11:begin
//      with cartType do
//      begin
//        id := $00;
//        name := 'ROM Only';
//        mbctype := 'ROM_ONLY';
//      end;
//    end;
//    $12:begin
//      with cartType do
//      begin
//        id := $00;
//        name := 'ROM Only';
//        mbctype := 'ROM_ONLY';
//      end;
//    end;
//    $13:begin
//      with cartType do
//      begin
//        id := $00;
//        name := 'ROM Only';
//        mbctype := 'ROM_ONLY';
//      end;
//    end;
  end;
end;

procedure TGBRom.SetLocation;
begin

end;

procedure TGBRom.SetRamSize;
begin
// ADDRESS_RAM_SIZE
  case ROMData[$149] of
    0:
      FRAMSize := 'None';
    1:
      FRAMSize := '2KB';
    2:
      FRAMSize := '8KB';
    3:
      FRAMSize := '32KB';
    4:
      FRAMSize := '128KB';
    5:
      FRAMSize := '64KB';
  else
    FRAMSize := 'None';
  end;
end;

procedure TGBRom.SetRomSize;
begin
  case ROMData[$148] of
    2:
      begin

      end;
    4:
      begin

      end;
    8:
      begin

      end;
    16:
      begin

      end;
  end;
//        Rom32KB (0x00, 2),
//        Rom64KB (0x01, 4),
//        Rom128KB(0x02, 8),
//        Rom256KB(0x03, 16),
//        Rom512KB(0x04, 32),
//        Rom1MB  (0x05, 64),
//        Rom2MB  (0x06, 128),
//        Rom4MB  (0x07, 256),
//        Rom8MB  (0x08, 512);
end;

procedure TGBRom.SetTitle;
begin
  FTitle := '';
  for var I := $134 to $143 do
  begin
    if ROMData[I] = 0 then
      Continue;
    FTitle := FTitle + Char(ROMData[I]);
  end;
end;

end.

