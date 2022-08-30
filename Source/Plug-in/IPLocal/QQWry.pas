unit QQWry;

interface

uses
  Windows, Messages, SysUtils, Classes;

type
  { 定义IP结构 }
  TIprecord = record
    a1: BYTE;
    a2: BYTE;
    a3: BYTE;
    a4: BYTE;
  end;

  { 查询纯真IP数据库记录 }
  TQQWry = class
  private
    FRecordCount: Integer;
    (* 首个IP起始位置 *)
    FFirstRecord: Integer;
    (* 加载数据库的流 *)
    FWryDatStream: TMemoryStream;
    (* 读偏移所在位置字符串以 #0 结束*)
    function ReadStrByPosition(Offset: Integer): string;
    (* 将一个字符IP转为整数 *)
    function StrIpToInt(str: string): Cardinal;
    (* 读偏移所在位置区域 *)
    function ReadAreaStrByPosition(offset: Integer): string;
    (* 取位置 = 国家 + 地区 *)
    function GetIpLocationByPosition(offset: Integer): string;
    (* 读取记录时用的 *)
    function GetIpCommonFunc(RecordNum: Integer; var StartIp, EndIp: Cardinal; Flags: Byte = 0): Integer;
  public
    (* 初始创建读取文件名 *)
    constructor Create(WryDatFileName: string);
    destructor Destroy; override;
    (* 根据提供的IP地址查找地区 *)
    function GetLocationByIpAddress(Ip: string): string;
    (* 读取版本信息 *)
    function GetVersion: string;
    (* 属性： 返回本个数据库中IP数 *)
    property RecordCount: Integer read FRecordCount;
    (* 将一个整数转为文本IP *)
    function IntIpToStr(IntIp: Cardinal; BigEndian: Boolean = False): string;
  end;

var
  g_TQQWry: TQQWry;

implementation

const
  { 重定义模式 }
  REDIRECT_MODE_1 = 1;
  REDIRECT_MODE_2 = 2;

{ TWryData }

constructor TQQWry.Create(WryDatFileName: string);
var
  LastRecord: Integer;
begin
  FRecordCount := 0;
  FWryDatStream := TMemoryStream.Create;
  if FileExists(WryDatFileName) then
  begin
    try
      FWryDatStream.LoadFromFile(WryDatFileName);
      FWryDatStream.Position := 0;
      FWryDatStream.Read(FFirstRecord, 4);
      FWryDatStream.Read(LastRecord, 4);
      FRecordCount := (LastRecord - FFirstRecord) div 7;
    except
      raise Exception.Create('加载纯真IP数据库错误!');
    end;
  end
  else
    raise Exception.Create('文件不存在，无法打开！');
end;

destructor TQQWry.Destroy;
begin
  FWryDatStream.Clear;
  FWryDatStream.Free;
  inherited Destroy;
end;


function TQQWry.ReadStrByPosition(offset: Integer): string;
var
  C: Byte;
{$IFDEF UNICODE}
  retstr: AnsiString;
{$ENDIF}
begin
  Result := '';
{$IFDEF UNICODE}
  retstr := '';
{$ENDIF}
  FWryDatStream.Seek(offset, soBeginning);
  FWryDatStream.Read(C, 1);
  while C <> 0 do
  begin
    {$IFNDEF UNICODE}
    Result := Result + Chr(C);
    {$ELSE}
    retstr := retstr + AnsiChar(C);
    {$ENDIF}
    FWryDatStream.Read(C, 1);
  end;
  {$IFDEF UNICODE}
  Result := string(retstr);
  {$ENDIF}
end;

function TQQWry.IntIpToStr(IntIp: Cardinal; BigEndian: Boolean): string;
var
  Ip: TIpRecord;
begin
  Ip := TIprecord(IntIp);
  case BigEndian of
    False:
      Result := Format('%d.%d.%d.%d', [Ip.a4, Ip.a3, Ip.a2, Ip.a1]);
    True:
      Result := Format('%d.%d.%d.%d', [Ip.a1, Ip.a2, Ip.a3, Ip.a4]);
  end;
end;

function TQQWry.StrIpToInt(str: string): Cardinal;
var
  StrIp: TStringList;
  IpR: TIprecord;
begin
  StrIp := TStringList.Create;
  try
    ExtractStrings(['.'], [' '], PChar(str), StrIp);
    if StrIp.Count = 4 then
    begin
      IpR.a1 := StrToIntDef(StrIp[3], 0);
      IpR.a2 := StrToIntDef(StrIp[2], 0);
      IpR.a3 := StrToIntDef(StrIp[1], 0);
      IpR.a4 := StrToIntDef(StrIp[0], 0);
      Result := Cardinal(IpR);
    end
    else
      Result := $0000000;
  finally
    StrIp.Free;
  end;
end;

function TQQWry.GetIpCommonFunc(RecordNum: Integer; var StartIp, EndIp: Cardinal; Flags: Byte = 0): Integer;
begin
  Result := FFirstRecord + RecordNum * 7;
  FWryDatStream.Seek(Result, soBeginning);
  FWryDatStream.Read(StartIp, 4);
  if Flags = 1 then
  begin
    FWryDatStream.Read(Result, 3);
    FWryDatStream.Seek(Result, sobeginning);
    FWryDatStream.Read(EndIp, 4);
  end;
end;

function TQQWry.ReadAreaStrByPosition(offset: Integer): string;
var
  b: Byte;
  AreaOffset: Integer;
begin
  FWryDatStream.Seek(offset, soBeginning);
  FWryDatStream.Read(b, 1);
  if (b = REDIRECT_MODE_1) or (b = REDIRECT_MODE_2) then
  begin
    FWryDatStream.Seek(offset + 1, soBeginning);
    FWryDatStream.Read(AreaOffset, 3);
    if AreaOffset = 0 then
      Result := '未知区域'
    else
      Result := ReadStrByPosition(AreaOffset);
  end
  else
    Result := ReadStrByPosition(offset);
end;

function TQQWry.GetIpLocationByPosition(offset: Integer): string;
var
  b: Byte;
  CountryOffset, CountryOffset2: Integer;
  CountryName: string;
  AreaName: string;
begin
  try
    FWryDatStream.Seek(offset + 4, soBeginning);
    FWryDatStream.Read(b, 1);
    case b of
      REDIRECT_MODE_1:
        begin
          CountryOffset := 0;
          FWryDatStream.Read(CountryOffset, 3);
          FWryDatStream.Seek(CountryOffset, soBeginning);
          FwryDatStream.Read(b, 1);
          if b = REDIRECT_MODE_2 then
          begin
            CountryOffset2 := 0;
            FWryDatStream.Read(CountryOffset2, 3);
            CountryName := ReadStrByPosition(CountryOffset2);
            FWryDatStream.Seek(CountryOffset + 4, soBeginning);
          end
          else
            CountryName := ReadStrByPosition(CountryOffset);

          AreaName := ReadAreaStrByPosition(FWryDatStream.Position);
        end;
      REDIRECT_MODE_2:
        begin
          CountryOffset := 0;
          FWryDatStream.Read(CountryOffset, 3);
          CountryName := ReadStrByPosition(CountryOffset);
          AreaName := ReadAreaStrByPosition(offset + 8);
        end;
    else
      CountryName := ReadStrByPosition(FWryDatStream.Position - 1);
      AreaName := ReadAreaStrByPosition(FWryDatStream.Position);
    end;
  except
    Result := '异常';
    Exit;
  end;
  Result := CountryName + AreaName;
end;

{ 二分查找 }
function TQQWry.GetLocationByIpAddress(Ip: string): string;
const
  returnstr = '未知';
var
  IpInt: Cardinal;
  Min, Max, MidRNo: Integer;
  StartIp: Cardinal;
  EndIp: Cardinal;
  Offset: Integer;
begin
  if FRecordCount > 0 then
  begin
    IpInt := StrIpToInt(Ip);
    if IpInt <> 0 then
    begin
      Min := 0;
      Max := FRecordCount - 1;
      while Min <= Max do
      begin
        MidRNo := (Min + Max) div 2;
        GetIpCommonFunc(MidRNo, StartIp, EndIp);
        if IpInt = StartIp then
        begin
          Max := MidRNo;
          Break;
        end
        else if IpInt > StartIp then
          Min := MidRNo + 1
        else
          Max := MidRNo - 1;
      end;

      Offset := GetIpCommonFunc(Max, StartIp, EndIp, 1);
      if (StartIp <= IpInt) and (EndIp >= IpInt) then
        Result := GetIpLocationByPosition(Offset)
      else
        Result := returnstr;
    end
    else
      Result := returnstr;
  end
  else
    Result := returnstr;
end;

function TQQWry.GetVersion: string;
var
  StartIp, EndIp: Cardinal;
  offset: Integer;
begin
  Result := '未知版本信息';
  if FRecordCount > 0 then
  begin
    offset := GetIpCommonFunc(FRecordCount, StartIp, EndIp, 1);
    if offset > 0 then
      Result := GetIpLocationByPosition(offset);
  end;
end;

 { 以下用的是顺序查找法 }
{
function TWryData.GetLocationByIpAddress(Ip: string):string;
var
  IpInt : Cardinal;
  I: Integer;
  StartIp: Cardinal;
  EndIp: Cardinal;
  Offset: Integer;
begin
  if FRecordCount > 0 then
  begin
    IpInt := StrIpToInt(Ip);
    if IpInt <> 0 then
    begin
      for I := 0 to FRecordCount - 1 do
      begin
        Offset := FFirstRecord + I * 7;
        FWryDatStream.Seek(Offset, soBeginning);
        FWryDatStream.Read(StartIp, 4);
        FWryDatStream.Read(Offset, 3);
        FWryDatStream.Seek(Offset, sobeginning);
        FWryDatStream.Read(EndIP, 4);
        if IpInt = StartIp then
        begin
          Result := GetIpLocationByPosition(Offset);
          break;
        end else if (IpInt > StartIp) and (IpInt < EndIp) then
        begin
          Result :=  GetIpLocationByPosition(Offset);
          break;
        end;
      end;
    end else Result := 'IP格式错误';
  end else Result := '当前数据库可能不是纯真数据库';
end;
}

end.

