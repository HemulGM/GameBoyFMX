unit GBSound;

interface

uses
  GBMemory, GBSoundChannel, System.Classes, SysUtils{, Winapi.MMSystem,
  Winapi.Windows};

const
  SAMPLE_RATE = 44100;
  SAMPLE_SIZE = 2;
//type TIntegerArray = array of Integer;
//type PIntegerArray = ^TIntegerArray;
//type PBytes = ^TBytes;

type
  TGBSound = class
  private
    _PGBMem: PGBMemory;
    soundBuffer: array[0..3, 0..749] of Integer;
    soundBufferMix: TBytes;
//    soundBufferIndex: Integer;
//    soundTimer: Integer;
//    channel1: TSquareWaveChannel;
//    channel2: TSquareWaveChannel;
//    channel3: TWaveChannel;
//    channel4: TNoiseChannel;
  // NR系列寄存器地址
    const
      NR10: Integer = $FF10;
      NR11: Integer = $FF11;
      NR12: Integer = $FF12;
      NR13: Integer = $FF13;
      NR14: Integer = $FF14;
      NR21: Integer = $FF16;
      NR22: Integer = $FF17;
      NR23: Integer = $FF18;
      NR24: Integer = $FF19;
      NR30: Integer = $FF1A;
      NR31: Integer = $FF1B;
      NR32: Integer = $FF1C;
      NR33: Integer = $FF1D;
      NR34: Integer = $FF1E;
      NR41: Integer = $FF20;
      NR42: Integer = $FF21;
      NR43: Integer = $FF22;
      NR44: Integer = $FF23;
      NR50: Integer = $FF24;
      NR51: Integer = $FF25;
      NR52: Integer = $FF26;

    procedure initChannels();
    procedure initChannel1();
    procedure updateChannel1();
    procedure initChannel2();
    procedure updateChannel2();
    procedure initChannel3();
    procedure updateChannel3();
    procedure initChannel4();
    procedure updateChannel4();
    procedure mixSound();

    //将读内存声音部分单独写
    function isSoundReset(soundNum: Integer): Boolean;
    procedure removeSoundReset(soundNum: Integer);
    function isAllSoundOn(): Boolean;
    procedure setSoundOn(soundNum: Integer);
    procedure setSoundOff(soundNum: Integer);
    function isSoundToTerminal(soundNum, soundOutput: Integer): Boolean;
    function getSoundLevel(soundOutput: Integer): Integer;
    procedure _debug_writeDataToFile(debug_i: Integer);
  public
    _debug_i: Integer;
    soundBufferIndex: Integer;
    soundTimer: Integer;
    channel1: TSquareWaveChannel;
    channel2: TSquareWaveChannel;
    channel3: TWaveChannel;
    channel4: TNoiseChannel;
    procedure startAudio();
    procedure updateSound(cycle: Integer);
    procedure playSound();
    constructor Create(PGBMem: PGBMemory); overload;
  end;

type
  PGBSound = ^TGBSound;

implementation

// 定义播放音乐要用的变量
//type PBytes = ^TBytes;
   {
var
  wh: TWaveHdr;
  hOut: HWAVEOUT;
  fmt: TWaveFormatEx;    }

{ TGBSound }

constructor TGBSound.Create(PGBMem: PGBMemory);
begin
  _PGBMem := @PGBMem^;

  channel1 := TSquareWaveChannel.Create;
  channel2 := TSquareWaveChannel.Create;
  channel3 := TWaveChannel.Create;
  channel4 := TNoiseChannel.Create;

  SetLength(soundBufferMix, 44100);
              {
  ZeroMemory(@fmt, SizeOf(TWaveFormatEx));
  fmt.wFormatTag := 1;
  fmt.nChannels := 2;
  fmt.nSamplesPerSec := 44100;
  fmt.nAvgBytesPerSec := 88200;
  fmt.nBlockAlign := 2;
  fmt.wBitsPerSample := 8;
  fmt.cbSize := 0;
                  }
  startAudio;

  _debug_i := 0;
end;

function TGBSound.getSoundLevel(soundOutput: Integer): Integer;
var
  tmp: Integer;
begin
  tmp := _PGBMem^.ReadGBMemory(NR50);
  Result := (tmp shr ((soundOutput - 1) * 4)) and $7;
end;

procedure TGBSound.initChannel1;
var
  _nr10, _nr11, _nr12, _nr13, _nr14: Integer;
  volume: TEnvelope;
begin
  if isSoundReset(1) then
  begin
    removeSoundReset(1);
    setSoundOn(1);
    _nr10 := _PGBMem^.ReadGBMemory(NR10);
    _nr11 := _PGBMem^.ReadGBMemory(NR11);
    _nr12 := _PGBMem^.ReadGBMemory(NR12);
    _nr13 := _PGBMem^.ReadGBMemory(NR13);
    _nr14 := _PGBMem^.ReadGBMemory(NR14);
    channel1.setOn(True);
    channel1.setWaveDuty((_nr11 shr 6) and $3);
    channel1.setIndex(0);
//    (nr13 | ((nr14 & 0x7) << 8)) & 0x7FF
    channel1.setGBFreq((_nr13 or ((_nr14 and $7) shl 8)) and $7FF);
    // 可能后期会有问题的？？debug
//    if soundBufferIndex=43 then
//      soundBufferIndex:=43;
    channel1.setFreq(131072 / (2048 - channel1.getGBFreq()));

    if ((_nr14 and $40) = $40) then
    begin
      channel1.setCount(True);
      channel1._setLength(((64 - (_nr11 and $3F)) * SAMPLE_RATE) div 256);
    end
    else
    begin
      channel1.setCount(False);
    end;
    volume := TEnvelope.Create;
    volume.setBase((_nr12 shr 4) and $0F);
    if (_nr12 and $8) = $8 then
      volume.setDirection(1)
    else
      volume.setDirection(0);
    volume.setStepLength((_nr12 and $7) * SAMPLE_RATE div 64);
    volume.setIndex(volume.getStepLength());
    channel1.setVolume(volume);
    channel1.setSweepLength(((_nr10 shr 4) and $7) * SAMPLE_RATE div 128);
    channel1.setSweepIndex(channel1.getSweepLength);
    if ((_nr10 and $8) = $8) then
      channel1.setSweepDirection(-1)
    else
      channel1.setSweepDirection(1);
    channel1.setSweepShift(_nr10 and $7);
  end;
end;

procedure TGBSound.initChannel2;
var
  _nr21, _nr22, _nr23, _nr24, freqX: Integer;
  volume: TEnvelope;
begin
  if isSoundReset(2) then
  begin
    removeSoundReset(2);
    setSoundOn(2);
    _nr21 := _PGBMem^.ReadGBMemory(NR21);
    _nr22 := _PGBMem^.ReadGBMemory(NR22);
    _nr23 := _PGBMem^.ReadGBMemory(NR23);
    _nr24 := _PGBMem^.ReadGBMemory(NR24);

    channel2.setOn(True);
    channel2.setWaveDuty((_nr21 shr 6) and $3);
    channel2.setIndex(0);

    freqX := _nr23 or ((_nr24 and $7) shl 8);
    channel2.setFreq(131072 / (2048 - freqX));

    if ((_nr24 and $40) = $40) then
    begin
      channel2.setCount(True);
      channel2._setLength(((64 - (_nr21 and $3F)) * SAMPLE_RATE) div 256);
    end
    else
    begin
      channel2.setCount(False);
    end;
    volume := TEnvelope.Create;
    volume.setBase((_nr22 shr 4) and $0F);
    if (_nr22 and $8) = $8 then
      volume.setDirection(1)
    else
      volume.setDirection(0);
    volume.setStepLength((_nr22 and $7) * SAMPLE_RATE div 64);
    volume.setIndex(volume.getStepLength());
    channel2.setVolume(volume);
  end;
end;

procedure TGBSound.initChannel3;
var
  _nr30, _nr31, _nr33, _nr34, freqX3: Integer;
  channel3wav: TIntegerArray;
  I: Integer;
begin
  if isSoundReset(3) then
  begin
    removeSoundReset(3);
    setSoundOn(3);
    channel3.setIndex(0);
    _nr30 := _PGBMem^.ReadGBMemory(NR30);
    _nr31 := _PGBMem^.ReadGBMemory(NR31);
    _nr33 := _PGBMem^.ReadGBMemory(NR33);
    _nr34 := _PGBMem^.ReadGBMemory(NR34);
    if (_nr30 and $80) = $80 then
      channel3.setOn(True)
    else
      channel3.setOn(False);
    freqX3 := _nr33 or ((_nr34 and $7) shl 8);
    channel3.setFreq(65536 / (2048 - freqX3));
    SetLength(channel3wav, 32);
    for I := $FF30 to $FF3F do
    begin
      channel3wav[((I - $FF30) * 2)] := ((_PGBMem^.ReadGBMemory(I) shr 4) and $F);
      channel3wav[((I - $FF30) * 2) + 1] := (_PGBMem^.ReadGBMemory(I) and $F);
    end;
    channel3.setWave(channel3wav);
    if ((_nr34 and $40) = $40) then
    begin
      channel3.setCount(True);
      channel3._setLength((256 - _nr31) * SAMPLE_RATE div 256);
    end
    else
    begin
      channel3.setCount(False);
    end;
  end;
end;

procedure TGBSound.initChannel4;
var
  _nr41, _nr42, _nr43, _nr44: Integer;
  volume: TEnvelope;
  tmp: Double;
begin
  if isSoundReset(4) then
  begin
    removeSoundReset(4);
    setSoundOn(4);
    _nr41 := _PGBMem^.ReadGBMemory(NR41);
    _nr42 := _PGBMem^.ReadGBMemory(NR42);
    _nr43 := _PGBMem^.ReadGBMemory(NR43);
    _nr44 := _PGBMem^.ReadGBMemory(NR44);
    channel4.setOn(True);
    channel4.setIndex(0);
    if (_nr44 and $40) = $40 then
    begin
      channel4.setCount(True);
      channel4._setLength((64 - (_nr41 and $3F)) * SAMPLE_RATE div 256);
    end
    else
    begin
      channel4.setCount(False);
    end;
    volume := TEnvelope.Create;
    volume.setBase((_nr42 shr 4) and $0F);
    if (_nr42 and $8) = $48 then
      volume.setDirection(1)
    else
      volume.setDirection(0);
    volume.setStepLength((_nr42 and $7) * SAMPLE_RATE div 64);
    volume.setIndex(volume.getStepLength);
    channel4.setVolume(volume);
    channel4.setShiftFreq(((_nr43 shr 4) and $F) + 1);
    if (_nr43 and $8) = $8 then
      channel4.setCounterStep(1)
    else
      channel4.setCounterStep(0);
    channel4.setDivRatio(_nr43 and $7);
    if channel4.getDivRatio = 0 then
      channel4.setDivRatio(0.5);
    tmp := 524288 / channel4.getDivRatio;
    channel4.setFreq(Trunc(tmp) shr channel4.getShiftFreq);
  end;
end;

procedure TGBSound.initChannels;
begin
  initChannel1();
  initChannel2();
  initChannel3();
  initChannel4();
end;

function TGBSound.isAllSoundOn: Boolean;
var
  tmp: Integer;
begin
  tmp := _PGBMem^.ReadGBMemory(NR52);
  if (tmp and $80) = $80 then
    Result := True
  else
    Result := False;
end;

function TGBSound.isSoundReset(soundNum: Integer): Boolean;
var
  tmp: Integer;
begin
  case soundNum of
    1:
      begin
        tmp := _PGBMem^.ReadGBMemory(NR14);
        if tmp and $80 = $80 then
          Result := True
        else
          Result := False;
      end;
    2:
      begin
        tmp := _PGBMem^.ReadGBMemory(NR24);
        if tmp and $80 = $80 then
          Result := True
        else
          Result := False;
      end;
    3:
      begin
        tmp := _PGBMem^.ReadGBMemory(NR34);
        if tmp and $80 = $80 then
          Result := True
        else
          Result := False;
      end;
    4:
      begin
        tmp := _PGBMem^.ReadGBMemory(NR44);
        if tmp and $80 = $80 then
          Result := True
        else
          Result := False;
      end;
  else
    Result := False;
  end;
end;

function TGBSound.isSoundToTerminal(soundNum, soundOutput: Integer): Boolean;
var
  mask, tmp: Integer;
begin
  mask := 1 shl ((soundNum - 1) + (soundOutput - 1) * 4);
  tmp := _PGBMem^.ReadGBMemory(NR51);
  if (tmp and mask) = mask then
    Result := True
  else
    Result := False;
end;

procedure TGBSound.mixSound;
var
  leftAmp, rightAmp: Integer;
begin
  if (soundBufferIndex = 567) then
    soundBufferIndex := 567;
  leftAmp := 0;
  if (isSoundToTerminal(1, 2) and channel1.isOn) then
    leftAmp := leftAmp + soundBuffer[0][soundBufferIndex];
  if (isSoundToTerminal(2, 2) and channel2.isOn) then
    leftAmp := leftAmp + soundBuffer[1][soundBufferIndex];
  if (isSoundToTerminal(3, 2) and channel3.isOn) then
    leftAmp := leftAmp + soundBuffer[2][soundBufferIndex];
  if (isSoundToTerminal(4, 2) and channel4.isOn) then
    leftAmp := leftAmp + soundBuffer[3][soundBufferIndex];
  leftAmp := leftAmp * getSoundLevel(2);
  leftAmp := leftAmp div 4;

  rightAmp := 0;
  if (isSoundToTerminal(1, 1) and channel1.isOn) then
    rightAmp := rightAmp + soundBuffer[0][soundBufferIndex];
  if (isSoundToTerminal(2, 1) and channel2.isOn) then
    rightAmp := rightAmp + soundBuffer[1][soundBufferIndex];
  if (isSoundToTerminal(3, 1) and channel3.isOn) then
    rightAmp := rightAmp + soundBuffer[2][soundBufferIndex];
  if (isSoundToTerminal(4, 1) and channel4.isOn) then
    rightAmp := rightAmp + soundBuffer[3][soundBufferIndex];
  rightAmp := rightAmp * getSoundLevel(1);
  rightAmp := rightAmp div 4;
  if leftAmp > 127 then
    leftAmp := 127;
  if rightAmp > 127 then
    rightAmp := 127;
  if leftAmp < -127 then
    leftAmp := -127;
  if rightAmp < -127 then
    rightAmp := -127;
  soundBufferMix[(soundBufferIndex * 2)] := leftAmp;
  soundBufferMix[(soundBufferIndex * 2) + 1] := rightAmp;
end;

procedure TGBSound.playSound();
begin      {
  ZeroMemory(@wh, SizeOf(TWaveHdr)); // test
  wh.lpData := PAnsiChar(soundBufferMix);
  wh.dwBufferLength := Length(soundBufferMix);
  wh.dwBytesRecorded := 0;
  wh.dwUser := 0;
  wh.dwFlags := WHDR_BEGINLOOP or WHDR_ENDLOOP;
  wh.dwLoops := 1;
  wh.lpNext := nil;
  wh.reserved := 0;

  waveOutOpen(@hOut, WAVE_MAPPER, @fmt, 0, 0, CALLBACK_NULL);
  waveOutPrepareHeader(hOut, @wh, SizeOf(TWaveHdr));
  waveOutWrite(hOut, @wh, SizeOf(TWaveHdr));    }
end;

procedure TGBSound.removeSoundReset(soundNum: Integer);
var
  tmp: Integer;
begin
  case soundNum of
    1:
      begin
        tmp := _PGBMem^.ReadGBMemory(NR14);
        _PGBMem^.WriteGBMemory(NR14, tmp and $7F);
      end;
    2:
      begin
        tmp := _PGBMem^.ReadGBMemory(NR24);
        _PGBMem^.WriteGBMemory(NR24, tmp and $7F);
      end;
    3:
      begin
        tmp := _PGBMem^.ReadGBMemory(NR34);
        _PGBMem^.WriteGBMemory(NR34, tmp and $7F);
      end;
    4:
      begin
        tmp := _PGBMem^.ReadGBMemory(NR44);
        _PGBMem^.WriteGBMemory(NR44, tmp and $7F);
      end;
  end;
end;

procedure TGBSound.setSoundOff(soundNum: Integer);
var
  tmp, mask: Integer;
begin
  mask := 1 shl (soundNum - 1);
  tmp := _PGBMem^.ReadGBMemory(NR52);
  _PGBMem^.WriteGBMemory(NR52, tmp and (not mask));
end;

procedure TGBSound.setSoundOn(soundNum: Integer);
var
  tmp, mask: Integer;
begin
  mask := 1 shl (soundNum - 1);
  tmp := _PGBMem^.ReadGBMemory(NR52);
  _PGBMem^.WriteGBMemory(NR52, tmp or mask);
end;

procedure TGBSound.startAudio;
begin
  soundTimer := 0;
  //line.start?? play sound;

  channel1.setOn(False);
  channel2.setOn(False);
  channel3.setOn(False);
  channel4.setOn(False);
  soundBufferIndex := 0;
end;

procedure TGBSound.updateChannel1;
var
  i, value, tmp2: Integer;
  tmp: Double;
//  byte_tmp: Byte;
begin
  if channel1.isOn then
  begin
    channel1.incIndex;
    //            int i = (int) ((32 * channel1.getFreq() * channel1.getIndex()) / SAMPLE_RATE) % 32;
    tmp := ((32 * channel1.getFreq * channel1.getIndex) / SAMPLE_RATE); // mod 32;
    i := Trunc(tmp) mod 32;
    value := channel1.getWave()[i];
    //debug
    if (soundBufferIndex = 567) then
      soundBufferIndex := 567;
//    byte_tmp :=  value * channel1.getVolume.getBase;
    soundBuffer[0][soundBufferIndex] := value * channel1.getVolume.getBase;
    if (channel1.isCount) and (channel1._getLength > 0) then
    begin
      channel1.decLength;
      if (channel1._getLength = 0) then
      begin
        channel1.setOn(False);
        setSoundOff(1);
      end;
    end;
    channel1.getVolume.handleSweep;
    if (channel1.getSweepIndex > 0) and (channel1.getSweepLength > 0) then
    begin
      channel1.decSweepIndex;
      if (channel1.getSweepIndex = 0) then
      begin
        channel1.setSweepIndex(channel1.getSweepLength);
        channel1.setFreq(channel1.getGbFreq() + (channel1.getGbFreq() shr channel1.getSweepShift()) * channel1.getSweepDirection());
        if channel1.getGBFreq > 2047 then
        begin
          channel1.setOn(False);
          setSoundOff(1);
        end
        else
        begin
          _PGBMem^.WriteGBMemory(NR13, channel1.getGBFreq and $FF);
          tmp2 := (_PGBMem^.ReadGBMemory(NR14) and $F8) or ((channel1.getGBFreq shr 8) and $7);
          _PGBMem^.WriteGBMemory(NR14, tmp2);
          channel1.setFreq(131072 / (2048 - channel1.getGbFreq()));
        end;
      end;
    end;
  end;
end;

procedure TGBSound.updateChannel2;
var
  i, value: Integer;
  tmp: Double;
begin
  if channel2.isOn then
  begin
    channel2.incIndex;
    tmp := ((32 * channel2.getFreq() * channel2.getIndex()) / SAMPLE_RATE);
    i := Trunc(tmp) mod 32;
    value := channel2.getWave()[i];
    soundBuffer[1][soundBufferIndex] := value * channel2.getVolume().getBase();
    if (channel2.isCount) and (channel2._getLength > 0) then
    begin
      channel2.decLength;
      if channel2._getLength = 0 then
      begin
        channel2.setOn(False);
        setSoundOff(2);
      end;
    end;
    channel2.getVolume.handleSweep;
  end;
end;

procedure TGBSound.updateChannel3;
var
  _nr30, _nr32, i, value: Integer;
  tmp: Double;
begin
  if channel3.isOn then
  begin
    _nr30 := _PGBMem^.ReadGBMemory(NR30);
    _nr32 := _PGBMem^.ReadGBMemory(NR32);
    channel3.incIndex;

    tmp := ((32 * channel3.getFreq() * channel3.getIndex()) / 44100);
    i := Trunc(tmp) mod 32;
    value := channel3.getWave()[i];
    if (_nr32 and $60) <> $0 then
      value := value shr (((_nr32 shr 5) and $3) - 1)
    else
      value := 0;
    value := value shl 1;
    if (_nr30 and $80) = $80 then
      soundBuffer[2][soundBufferIndex] := value - $F
    else
      soundBuffer[2][soundBufferIndex] := 0;
    if (channel3.isCount) and (channel3._getLength > 0) then
    begin
      channel3.decLength;
      if channel3._getLength = 0 then
      begin
        channel3.setOn(False);
        setSoundOff(3);
      end;
    end;
  end;
end;

procedure TGBSound.updateChannel4;
var
  i: Integer;
  tmp: Double;
  value: Byte;
begin
  if channel4.isOn then
  begin
    channel4.incIndex;
    tmp := (channel4.getFreq * channel4.getIndex) / SAMPLE_RATE;
    value := 0;
    if channel4.getCounterStep = 1 then
    begin
      i := Trunc(tmp) mod $7F;
      value := ((TNoiseChannel.noise7[i shr 3] shr (i and $7)) and $1);
    end
    else
    begin
      i := Trunc(tmp) mod $7FFF;
      value := ((TNoiseChannel.noise15[i shr 3] shr (i and $7)) and $1);
    end;
    soundBuffer[3][soundBufferIndex] := ((value * 2 - 1) * channel4.getVolume.getBase);
    if (channel4.isCount) and (channel4._getLength > 0) then
    begin
      channel4.decLength;
      if (channel4._getLength = 0) then
      begin
        channel4.setOn(False);
        setSoundOff(4);
      end;
    end;
    channel4.getVolume.handleSweep;
  end;
end;

procedure TGBSound.updateSound(cycle: Integer);
var
  numSamples: Integer;
  _buf: TBytes;
  I: Integer;
begin
  initChannels();

  soundTimer := soundTimer + cycle;
  if soundTimer >= 93 then
  begin
    soundTimer := soundTimer - 93;
    // 判断 内存 IOPort[$26] and $80 是否=$80
    if isAllSoundOn then
    begin
      updateChannel1();
//      updateChannel2();
//      updateChannel3();
//      updateChannel4();
      mixSound();
    end
    else
    begin
      soundBufferMix[(soundBufferIndex * 2)] := $00;
      soundBufferMix[(soundBufferIndex * 2) + 1] := $00;
    end;
    Inc(soundBufferIndex);
    if soundBufferIndex = 43 then
      soundBufferIndex := 43;
    if (soundBufferIndex >= 750) then
    begin
      numSamples := 1500; // because line.available = 44100,so set to 1500

//                if (1500 >= line.available() * 2) {
//
//                    numSamples = line.available() * 2;
//                } else {
//
//                    numSamples = 1500;
//                }
//
//                line.write(soundBufferMix, 0, numSamples);
//
      //将soundBufferMix数组，输出到文件，验证下数据！！！
//      for I := 0 to Length(soundBufferMix)-1 do
//        _buf[I] := soundBufferMix[I];

      playSound();
//      Inc(_debug_i);
//      if (_debug_i > 141) and (_debug_i<144) then
//        _debug_writeDataToFile(_debug_i);

//        playSound();
//      else if _debug_i =160 then
//        _debug_i := 160;
      if _debug_i = 142 then
        _debug_i := 142;

      soundBufferIndex := 0;
    end;
  end;
end;

procedure TGBSound._debug_writeDataToFile(debug_i: Integer);
var
  data: TFileStream;
  I: Integer;
begin
  data := TFileStream.Create('D:\gbsound\wjlgb_' + IntToStr(debug_i) + '.txt', fmCreate);
//  data := TFileStream.Create('d:\buffer.bin',fmCreate);
  try
//    SetLength(data,44100);
//    data.Seek(0);
    for I := 0 to 44099 do
      data.WriteBuffer(soundBufferMix[I], 1);
  finally
    data.Free;
  end;
end;

end.

