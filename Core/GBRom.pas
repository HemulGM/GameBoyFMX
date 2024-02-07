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
    romTitle: string; // ´æ·ÅromµÄÃû×Ö
    romLocate: string;
    ramSize: string;
    romSize: string;
    cartType: TCartridgeTypes;
    procedure setTitle();
    procedure setLocation();
    procedure setRamSize();
    procedure setRomSize();
    procedure setCartridgeType();
  public
    romData: array of byte;
    procedure readRom(data: TFileStream);
    procedure readBios(biosdata: TFileStream);
    function getTitle(): string;
    function getRamSize(): string;
    function getCartridgeType(): TCartridgeTypes;
  end;

type
  PGBRom = ^TGBRom;

implementation

{ TGBRom }

function TGBRom.getCartridgeType: TCartridgeTypes;
begin
  Result := cartType;
end;

function TGBRom.getRamSize: string;
begin
  Result := ramSize;
end;

function TGBRom.getTitle: string;
begin
  Result := romTitle;
end;

procedure TGBRom.readBios(biosdata: TFileStream);
begin
  if SizeOf(romData) > 0 then
  begin

  end;
end;

procedure TGBRom.readRom(data: TFileStream);
var
  I: Integer;
begin
  data.Position := 0;
  SetLength(romData, data.Size);
  for I := 0 to data.Size - 1 do
  begin
    data.ReadBuffer(romData[I], 1);
  end;
  setTitle;
  setRamSize;
  setCartridgeType;
end;

procedure TGBRom.setCartridgeType;
begin
  case romData[$147] of
    $00:
      begin
        with cartType do
        begin
          id := $00;
          name := 'ROM Only';
          mbctype := 'ROM_ONLY';
        end;
      end;
    $01:
      begin
        with cartType do
        begin
          id := $01;
          name := 'MBC1';
          mbctype := 'MBC1';
        end;
      end;
    $02:
      begin
        with cartType do
        begin
          id := $02;
          name := 'MBC1 + RAM';
          mbctype := 'MBC1';
        end;
      end;
    $03:
      begin
        with cartType do
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

procedure TGBRom.setLocation;
begin

end;

procedure TGBRom.setRamSize;
begin
// ADDRESS_RAM_SIZE
  case romData[$149] of
    0:
      begin
        ramSize := 'None';
      end;
    1:
      begin
        ramSize := '2KB';
      end;
    2:
      begin
        ramSize := '8KB';
      end;
    3:
      begin
        ramSize := '32KB';
      end;
    4:
      begin
        ramSize := '128KB';
      end;
    5:
      begin
        ramSize := '64KB';
      end;
  else
    ramSize := 'None';
  end;
end;

procedure TGBRom.setRomSize;
begin
  case romData[$148] of
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

procedure TGBRom.setTitle;
var
  I: Integer;
  s: string;
begin
  s := '';
  for I := $134 to $143 do
  begin
    if romData[I] = 0 then
      continue;
    s := s + Char(romData[I]);
  end;
  romTitle := s;
end;

end.

