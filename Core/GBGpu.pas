unit GBGpu;

interface

uses
  GBInterruptManager, System.Classes, System.Contnrs;

type
  Mode = (HBLANK, VBLANK, OAM_ACCESS, VRAM_ACCESS);

type
  TScreenArray = array[0..23039] of Integer;

  PScanlineRow = ^TScanlineRow;

  TDrawCallback = reference to procedure(const Value: TScreenArray);

  TScanlineRow = array[0..159] of Integer;

  TRGB32 = packed record
    B, G, R, A: Byte;
  end;

  TRGB32Array = packed array[0..MaxInt div SizeOf(TRGB32) - 1] of TRGB32;

  PRGB32Array = ^TRGB32Array;

type
  TSprite = record
    X: Integer;
    Y: Integer;
    tileNumber: Integer;
    belowBackground, isXflip, isYflip, isPalette1: Boolean;
  end;

type
  PSprite = ^TSprite;

type
  GBSprites = array[0..39] of TSprite;

type
  PGBSprites = ^GBSprites;

type
  TGBGpu = class
  private
    currentMode: Mode;

    //wjlGBColor: array[0..3] of Integer; // 定义4级灰度

    LcdStatus_isAnyStat, LcdStatus_lylycEnable, LcdStatus_oamAccessEnable, LcdStatus_vblankEnable, LcdStatus_hblankEnable, LcdStatus_isLyLyc: Boolean;

    LcdControl_lcdEnable, LcdControl_wndTileMapDisplaySelect, LcdControl_wndDisplayEnable, LcdControl_bgAndWndTileDataSelect, LcdControl_bgTileMapDisplaySelect, LcdControl_tallSpriteMode, LcdControl_spriteDisplayEnable, LcdControl_bgWndDisplayPriority: Boolean;
    function LcdControl_getSpriteSize(): Integer;

    procedure RenderScanLine();
    procedure renderBackground(pscanlineRow: PScanlineRow);
    procedure RenderSprites(pscanlineRow: PScanlineRow);

  public
    modeClock: Integer; //临时放开
    GBInterrupt: TGBInterruptManager;
    width, heigth: Integer;
    line, lyc: Integer;
    scrollX, scrollY: Integer;
    vram: array[0..$2000 - 1] of Integer;
    tileset: array[0..383, 0..7, 0..7] of Integer;
    screen: TScreenArray; //array[0..23039] of Integer;
    backgroundPalette: array[0..3] of Integer;
    spritePalette: array[0..1, 0..3] of Integer;
    palette: array[0..3] of Integer;

    spriteList: GBSprites;
    pspriteList: PGBSprites;
    FDrawCallback: TDrawCallback;

    procedure Step(Cycle: Integer);

    function LcdStatus_getLcdStatus(): Integer;
    procedure LcdStatus_setLcdStatus(value: Integer);
    procedure LcdStatus_processLcdStatus();

    procedure LcdControl_setLcdControl(value: Integer);
    function LcdControl_getLcdControl(): Integer;

    procedure updateTile(address: Integer);
    procedure buildSprite(address, value: Integer);

    constructor Create(Callback: TDrawCallback); reintroduce;
  end;

type
  PGBGpu = ^TGBGpu;

implementation

{ TGBGpu }

procedure TGBGpu.buildSprite(address, value: Integer);
var
  spriteNumber: Integer;
begin
  spriteNumber := address shr 2;
  if (spriteNumber < 40) then
  begin
    case (address and $3) of
      // Y-coordinate
      0:
        begin
          pspriteList^[spriteNumber].Y := value - 16;
        end;
      // X-coordinate
        1:
        begin
          pspriteList^[spriteNumber].X := value - 8;
        end;
      // Data tile
        2:
        begin
          pspriteList^[spriteNumber].tileNumber := value;
        end;
      // Options
        3:
        begin
          if (value and $10) <> 0 then
            pspriteList^[spriteNumber].isPalette1 := True
          else
            pspriteList^[spriteNumber].isPalette1 := False;
          if (value and $20) <> 0 then
            pspriteList^[spriteNumber].isXflip := True
          else
            pspriteList^[spriteNumber].isXflip := False;
          if (value and $40) <> 0 then
            pspriteList^[spriteNumber].isYflip := True
          else
            pspriteList^[spriteNumber].isYflip := False;
          if (value and $80) <> 0 then
            pspriteList^[spriteNumber].belowBackground := True
          else
            pspriteList^[spriteNumber].belowBackground := False;
        end;
    end;
  end;
end;

constructor TGBGpu.Create(Callback: TDrawCallback);
var
  I: Integer;
begin
  FDrawCallback := Callback;
  width := 160;
  heigth := 144;
  LcdStatus_isAnyStat := False;
  LcdStatus_lylycEnable := False;
  LcdStatus_oamAccessEnable := False;
  LcdStatus_vblankEnable := False;
  LcdStatus_hblankEnable := False;
  LcdStatus_isLyLyc := False;

  LcdControl_lcdEnable := true;                //Bit 7 - LCD Display Enable             (0=Off, 1=On)
  LcdControl_wndTileMapDisplaySelect := false; //Bit 6 - Window Tile Map Display Select (0=9800-9BFF, 1=9C00-9FFF)
  LcdControl_wndDisplayEnable := false;        //Bit 5 - Window Display Enable          (0=Off, 1=On)
  LcdControl_bgAndWndTileDataSelect := false;  //Bit 4 - BG & Window Tile Data Select   (0=8800-97FF, 1=8000-8FFF)
  LcdControl_bgTileMapDisplaySelect := false;  //Bit 3 - BG Tile Map Display Select     (0=9800-9BFF, 1=9C00-9FFF)
  LcdControl_tallSpriteMode := false;          //Bit 2 - OBJ (Sprite) Size              (0=8x8, 1=8x16)
  LcdControl_spriteDisplayEnable := false;     //Bit 1 - OBJ (Sprite) Display Enable    (0=Off, 1=On)
  LcdControl_bgWndDisplayPriority := false;    //Bit 0 - BG/Window Display/Priority     (0=Off, 1=On)

  backgroundPalette[0] := 0;
  backgroundPalette[1] := 3;
  backgroundPalette[2] := 3;
  backgroundPalette[3] := 3;
  spritePalette[0][0] := 0;
  spritePalette[0][1] := 3;
  spritePalette[0][2] := 3;
  spritePalette[0][3] := 3;
  spritePalette[1][0] := 0;
  spritePalette[1][1] := 3;
  spritePalette[1][2] := 3;
  spritePalette[1][3] := 3;
  palette[0] := 0;
  palette[1] := 1;
  palette[2] := 2;
  palette[3] := 3;

  modeClock := 0;
  currentMode := Mode.VRAM_ACCESS;
  lcdControl_setLcdControl($91);

//  wjlGBColor[0] := 255 Shl 16 or 255  shl 8 or 255;//OFF
//  wjlGBColor[1] := 192 Shl 16 or 192  shl 8 or 192;//LIGHT
//  wjlGBColor[2] := 96 Shl 16 or 96  shl 8 or 96;//DARK
//  wjlGBColor[3] := 40 Shl 16 or 40  shl 8 or 40;//ON
  // 初始化sprite
  for I := 0 to 39 do
  begin
    with spriteList[I] do
    begin
      y := 0; // Y-coordinate of top-left corner, (Value stored is Y-coordinate minus 16)
      x := 0; // X-coordinate of top-left corner, (Value stored is X-coordinate minus 8)
      tileNumber := 0;
      belowBackground := false; // false = above background, true = below background
      isYflip := false;
      isXflip := false;
      isPalette1 := false; // false = palette 0, true = palette 1
    end;
  end;
  pspriteList := @spriteList;
end;

function TGBGpu.LcdControl_getLcdControl: Integer;
var
  value: Integer;
begin
  value := 0;
  if LcdControl_lcdEnable then
    value := value or $80
  else
    value := value or 0;
  if LcdControl_wndTileMapDisplaySelect then
    value := value or $40
  else
    value := value or 0;
  if LcdControl_wndDisplayEnable then
    value := value or $20
  else
    value := value or 0;
  if LcdControl_bgAndWndTileDataSelect then
    value := value or $10
  else
    value := value or 0;
  if LcdControl_bgTileMapDisplaySelect then
    value := value or $8
  else
    value := value or 0;
  if LcdControl_tallSpriteMode then
    value := value or $4
  else
    value := value or 0;
  if LcdControl_spriteDisplayEnable then
    value := value or $2
  else
    value := value or 0;
  if LcdControl_bgWndDisplayPriority then
    value := value or $1
  else
    value := value or 0;
  Result := value;
end;

function TGBGpu.LcdControl_getSpriteSize: Integer;
begin
  if LcdControl_tallSpriteMode then
    Result := 16
  else
    Result := 8;
end;

procedure TGBGpu.LcdControl_setLcdControl(value: Integer);
var
  newLcdEnable, newWndTileMapDisplaySelect, newWndDisplayEnable, newBgAndWndTileDataSelect, newBgTileMapDisplaySelect, newTallSpriteMode, newSpriteDisplayEnable, newBgWndDisplayPriority: Boolean;
begin
  if (value and $80 <> 0) then
    newLcdEnable := True
  else
    newLcdEnable := False;
  if (value and $40 <> 0) then
    newWndTileMapDisplaySelect := True
  else
    newWndTileMapDisplaySelect := False;
  if (value and $20 <> 0) then
    newWndDisplayEnable := True
  else
    newWndDisplayEnable := False;
  if (value and $10 <> 0) then
    newBgAndWndTileDataSelect := True
  else
    newBgAndWndTileDataSelect := False;
  if (value and $8 <> 0) then
    newBgTileMapDisplaySelect := True
  else
    newBgTileMapDisplaySelect := False;
  if (value and $4 <> 0) then
    newTallSpriteMode := True
  else
    newTallSpriteMode := False;
  if (value and $2 <> 0) then
    newSpriteDisplayEnable := True
  else
    newSpriteDisplayEnable := False;
  if (value and $1 <> 0) then
    newBgWndDisplayPriority := True
  else
    newBgWndDisplayPriority := False;

  if (not LcdControl_lcdEnable) and (newLcdEnable) then
  begin
    line := 0;
    currentMode := Mode.HBLANK;
    modeClock := 0;
  end;
  LcdControl_lcdEnable := newLcdEnable;
  LcdControl_wndTileMapDisplaySelect := newWndTileMapDisplaySelect;
  LcdControl_wndDisplayEnable := newWndDisplayEnable;
  LcdControl_bgAndWndTileDataSelect := newBgAndWndTileDataSelect;
  LcdControl_bgTileMapDisplaySelect := newBgTileMapDisplaySelect;
  LcdControl_tallSpriteMode := newTallSpriteMode;
  LcdControl_spriteDisplayEnable := newSpriteDisplayEnable;
  LcdControl_bgWndDisplayPriority := newBgWndDisplayPriority;
end;

function TGBGpu.LcdStatus_getLcdStatus: Integer;
var
  value: Integer;
begin
  value := 0;
  value := value or $80; // bit 7 is unused and always returns 1
  if LcdStatus_lylycEnable then // bit 6 is ly==lyc enable
    value := value or $40
  else
    value := value or 0;
  if LcdStatus_oamAccessEnable then // bit 5 is mode 2 enable
    value := value or $20
  else
    value := value or 0;
  if LcdStatus_vblankEnable then  // bit 4 is mode 1 enable
    value := value or $10
  else
    value := value or 0;
  if LcdStatus_hblankEnable then // bit 3 is mode 0 enable
    value := value or $8
  else
    value := value or 0;
  if (line = lyc) then  // bit 2 is if ly currently equals lyc
    value := value or $4
  else
    value := value or 0;
  value := value or Ord(currentMode);
  Result := value;
end;

procedure TGBGpu.LcdStatus_processLcdStatus;
begin
  if line = lyc then
    LcdStatus_isLyLyc := True
  else
    LcdStatus_isLyLyc := False;
// from The Cycle Accurate Game Boy Document (TCAGBD.pdf)
  if (LcdStatus_isLyLyc and LcdStatus_lylycEnable) or
    ((Ord(currentMode) = 0) and LcdStatus_hblankEnable) or
    ((Ord(currentMode) = 2) and LcdStatus_oamAccessEnable) or
    ((Ord(currentMode) = 1) and (LcdStatus_oamAccessEnable or LcdStatus_vblankEnable)) then
  begin
    if not LcdStatus_isAnyStat then
    begin
      LcdStatus_isAnyStat := True;
      GBInterrupt.Instance.raiseInterruptByIdx(3); // LCDC_STATUS
    end;
  end
  else
  begin
    LcdStatus_isAnyStat := False;
  end;
end;

procedure TGBGpu.LcdStatus_setLcdStatus(value: Integer);
begin
  if (value and $40) <> 0 then// bit 6 is ly==lyc enable
    LcdStatus_lylycEnable := True
  else
    LcdStatus_lylycEnable := True;
  if (value and $20) <> 0 then// bit 5 is mode 2 enable
    LcdStatus_oamAccessEnable := True
  else
    LcdStatus_oamAccessEnable := False;
  if (value and $10) <> 0 then// bit 4 is mode 1 enable
    LcdStatus_vblankEnable := True
  else
    LcdStatus_vblankEnable := False;
  if (value and $8) <> 0 then// bit 3 is mode 0 enable
    LcdStatus_hblankEnable := True
  else
    LcdStatus_hblankEnable := False;
end;

procedure TGBGpu.renderBackground(pscanlineRow: pscanlineRow);
var
  bgmap, bgtile: Boolean;
  mapOffset, lineOffset, canvasOffset: Integer;
  x, y, colorint, tile: Integer;
  I: Integer;
begin
  bgmap := LcdControl_bgTileMapDisplaySelect;
  bgtile := not LcdControl_bgAndWndTileDataSelect;
  if (bgmap) then
    mapOffset := $1c00
  else
    mapOffset := $1800;
  mapOffset := mapOffset + ((((line + scrollY) and $FF) shr 3) shl 5);
  lineOffset := scrollX shr 3;
  x := scrollX and 7;
  y := (line + scrollY) and 7;
  canvasOffset := line * 160;
  tile := vram[mapOffset + lineOffset];
  if tile = 13 then
    tile := 13;
  if (bgtile and (tile < 128)) then
    tile := tile + 256;
  for I := 0 to 159 do
  begin
    colorint := tileset[tile][y][x];
    screen[canvasOffset] := backgroundPalette[colorint];

    canvasOffset := canvasOffset + 1;
    pscanlineRow^[I] := colorint;
    x := x + 1;
    if (x = 8) then
    begin
      x := 0;
      lineOffset := (lineOffset + 1) and 31;
      tile := vram[mapOffset + lineOffset];
      if (bgtile and (tile < 128)) then
        tile := tile + 256;
    end;
  end;
end;

procedure TGBGpu.RenderScanLine;
var
  scanlineRow: TScanlineRow;
begin
  FillChar(scanlineRow, SizeOf(scanlineRow), 0);
  if LcdControl_bgWndDisplayPriority then
    renderBackground(@scanlineRow);
  if LcdControl_spriteDisplayEnable then
    RenderSprites(@scanlineRow);
end;

procedure TGBGpu.RenderSprites(pscanlineRow: pscanlineRow);
var
  spriteSize, canvasoffs: Integer;
  I: Integer;
  pal: array[0..3] of Integer;
  J: Integer;
  tilerow: array[0..7] of Integer;
  color: Integer;
begin
  spriteSize := LcdControl_getSpriteSize();
  for I := 0 to 39 do
  begin
    // Check if this sprite falls on this scanline
    if ((pspriteList^[I].y <= line) and ((pspriteList^[I].y + LcdControl_getSpriteSize()) > line)) then
    begin
    // Palette to use for this sprite
      if (pspriteList^[I].isPalette1) then
      begin
        for J := 0 to 3 do
        begin
          pal[J] := spritePalette[1][J]
        end;
      end
      else
      begin
        for J := 0 to 3 do
        begin
          pal[J] := spritePalette[0][J]
        end;
      end;
      // Where to render on the canvas
      canvasoffs := ((line * 160) + pspriteList^[I].x);

      // If the sprite is Y-flipped,
      // use the opposite side of the tile
      if (pspriteList^[I].isYflip) then
      begin
        for J := 0 to 7 do
        begin
          tilerow[J] := tileset[pspriteList^[I].tileNumber][spriteSize - 1 - (line - pspriteList^[I].y)][J]
        end;
      end
      else
      begin
        for J := 0 to 7 do
        begin
          tilerow[J] := tileset[pspriteList^[I].tileNumber][line - pspriteList^[I].y][J];
        end;
      end;

      for J := 0 to 7 do
      begin
      // If this pixel is still on-screen, AND
      // if it's not colour 0 (transparent), AND
      // if this sprite has priority OR shows under the bg
      // then render the pixel
        if (((pspriteList^[I].x + J) >= 0) and ((pspriteList^[I].x + J) < 160)) and //(tilerow[J]<>0) and
          ((not pspriteList^[I].belowBackground) or (pscanlineRow^[pspriteList^[I].x + J] <= 0)) then
        begin
        // If the sprite is X-flipped,
        // write pixels in reverse order
          if pspriteList^[I].isXflip then
          begin
            if tilerow[7 - J] <> 0 then
            begin
              color := pal[tilerow[7 - J]];
              screen[canvasoffs] := color;
            end;
          end
          else
          begin
            if tilerow[J] <> 0 then
            begin
              color := pal[tilerow[J]];
              screen[canvasoffs] := color;
            end;
          end;

//        screen[canvasoffs] := color;
        end;
        canvasoffs := canvasoffs + 1;
      end;
    end;
  end;
end;

procedure TGBGpu.Step(Cycle: Integer);
begin
  modeClock := modeClock + Cycle;
  case currentMode of
    OAM_ACCESS:
      begin
        if (modeClock >= 80) then
        begin
          currentMode := Mode.VRAM_ACCESS;
          modeClock := 0;
        end;
      end;
    VRAM_ACCESS:
      begin
        if (modeClock >= 172) then
        begin
          currentMode := Mode.HBLANK;
          modeClock := 0;
          RenderScanLine();
        end;
      end;
    HBLANK:
      begin
        if (modeClock >= 204) then
        begin
          modeClock := 0;
          line := line + 1;
          if (line = 143) then
          begin
            currentMode := Mode.VBLANK;
            GBInterrupt.Instance.raiseInterruptByIdx(4); // VBLANK
          // 显示器渲染screen
            FDrawCallback(screen);
          end
          else
          begin
            currentMode := Mode.OAM_ACCESS;
          end;
        end;
      end;
    VBLANK:
      begin
        if (modeClock >= 456) then
        begin
          modeClock := 0;
          line := line + 1;
        end;
        if (line > 153) then
        begin
          currentMode := Mode.OAM_ACCESS;
          line := 0;
        end;
      end;
  end;
  LcdStatus_processLcdStatus();
end;

procedure TGBGpu.updateTile(address: Integer);
var
  tile, y, sx: Integer;
  I, val: Integer;
  tmp1, tmp2: Integer;
begin
  // get base address for this tile row
  address := address and $1FFE;
  // work out which tile and row was updated
  tile := (address shr 4) and 511;
  y := (address shr 1) and 7;
  for I := 0 to 7 do
  begin
  // find bit index for this pixel
    sx := 1 shl (7 - I);
    if (vram[address] and sx) <> 0 then
      tmp1 := 1
    else
      tmp1 := 0;
    if (vram[address + 1] and sx) <> 0 then
      tmp2 := 2
    else
      tmp2 := 0;
    val := tmp1 or tmp2;
    tileset[tile][y][I] := val;
  end;
end;

end.

