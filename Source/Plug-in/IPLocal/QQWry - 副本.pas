unit QQWry;

interface

uses
  Windows, SysUtils, Classes, Math;

const  //�����ڴ��ļ�ӳ���־
  QQWryMapFileTag = 'QQWryMapFile';

type
  TQQWry = class(TObject)
  public
    constructor Create(AQQWryFileName: string);
    destructor Destroy; override;
    function GetQQWryFileName: string;
    function GetQQWryFileSize: int64;
    function GetIPDataNum: int64;
    function GetQQWryDate: TDate;
    function GetQQWryDataFrom: string;
    procedure GetIPDataByIPRecordID(IPRecordID: int64; var IPData: PChar); overload;
    procedure GetIPLocationByEndIPOffset(EndIPOffset: int64; var IPLocation: PChar);
    procedure GetIPDataByIPRecordID(IPRecordID: int64; var IPData: TStringlist); overload;
    function GetIPLocalData(IPRecordID: int64): PChar;
    function GetIPValue(IP: string): int64;
    function GetIPDataID(IP: string): int64;
    function ExtractIPDataToTxtFile(ATxtFileName: string): integer;
  private
    QQWryFileName: string;
    QQWryFileSize: int64;
    IPDataNum: int64;
    FirstIPIndexOffset, LastIPIndexOffset: integer;
    hQQWryFile, hQQWryMapFile: THandle;
    pQQWryMapFile: Pointer;
    pQQWryPos: PByte;
    function GetFileSize(AFileName: string): int64;
  end;

var
  g_TQQWry: TQQWry;

implementation

///**
//* ��ȡ�ļ���С
//* @param  (AFileName) (�ļ�ȫ��)
//* @return (�ļ���С)
//*/
function TQQWry.GetFileSize(AFileName: string): int64;
var
  FileStream: TFileStream;
begin
  try
    FileStream := TFileStream.Create(AFileName, fmOpenRead);
  except
    raise Exception.Create(format('�ļ� %s �޷��򿪣�', [AFileName]));
    exit;
  end;
  result := FileStream.Size;
  FileStream.Free;
end;

///**
//* ���캯��������һ��TQQWry������IP���ݿ⴦�����
//* @param (AQQWryFileName) (����IP���ݿ��ļ���ȫ�ļ���)
//* @return ��
//*/
constructor TQQWry.Create(AQQWryFileName: string);
var
  Buffer: TOFStruct;
begin
  inherited Create;
  try
    QQWryFileName := AQQWryFileName;
    //���ļ�����
    if not FileExists(QQWryFileName) then
      raise Exception.Create(format('�ļ� %s �����ڣ�', [QQWryFileName]));

    //���ļ���С
    QQWryFileSize := GetFileSize(QQWryFileName);
    if QQWryFileSize = 0 then
      raise Exception.Create(format('�ļ� %s ��СΪ�գ�', [QQWryFileName]));

    //���ļ����
    hQQWryFile := OpenFile(PAnsiChar(AnsiString(QQWryFileName)), Buffer, OF_READWRITE);
    if hQQWryFile = HFILE_ERROR then
      raise Exception.Create(format('�ļ� %s ���ܴ򿪣�', [QQWryFileName]));

    //�����ļ�ӳ�����
    hQQWryMapFile := CreateFileMapping(hQQWryFile, nil, PAGE_READWRITE, 0, QQWryFileSize, PChar(QQWryMapFileTag));
    if hQQWryMapFile = 0 then
    begin
      CloseHandle(hQQWryFile);
      raise Exception.Create('���ܴ����ڴ�ӳ���ļ���');
    end;

    //��ȡӳ���ļ�ӳ���ַ
    pQQWryMapFile := MapViewOfFile(hQQWryMapFile, FILE_MAP_ALL_ACCESS, 0, 0, 0);
    if pQQWryMapFile = nil then
    begin
      CloseHandle(hQQWryFile);
      CloseHandle(hQQWryMapFile);
      raise Exception.Create('���ܻ�ȡ�ļ�ӳ���ַ��');
    end;

    pQQWryPos := pQQWryMapFile;
    FirstIPIndexOffset := PInteger(pQQWryPos)^;
    Inc(pQQWryPos, 4);
    LastIPIndexOffset := PInteger(pQQWryPos)^;
    IPDataNum := (LastIPIndexOffset - FirstIPIndexOffset) div 7 + 1;
  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message);
      exit;
    end;
  end;
end;

///**
//* �������� (�ͷ�TQQWry���󣬹ر��ļ�ӳ�䣬�ر��ļ�ӳ����������ر��ļ����)
//* @param  ��
//* @return ��
//*/
destructor TQQWry.Destroy;
begin
  if pQQWryMapFile <> nil then
    UnMapViewOfFile(pQQWryMapFile);  //�ر��ļ�ӳ��
  if hQQWryMapFile <> 0 then
    CloseHandle(hQQWryMapFile);        //�ر��ļ�ӳ�������
  if hQQWryFile <> 0 then
    CloseHandle(hQQWryFile);              //�ر��ļ����
  inherited Destroy;
end;

///**
//* ��ȡ����IP���ݿ��ļ���ȫ�ļ���
//* @param  ��
//* @return (����IP���ݿ��ļ���ȫ�ļ���)
//*/
function TQQWry.GetQQWryFileName: string;
begin
  Result := QQWryFileName;
end;

///**
//* ��ȡ����IP���ݿ��ļ���С
//* @param  ��
//* @return (����IP���ݿ��ļ���С)
//*/
function TQQWry.GetQQWryFileSize: int64;
begin
  Result := QQWryFileSize;
end;

///**
//* ��ȡ����IP���ݿ��ں��е�IP��ַ��Ϣ��¼��
//* @param  ��
//* @return (����IP���ݿ��¼��)
//*/
function TQQWry.GetIPDataNum: int64;
begin
  Result := IPDataNum;
end;

///**
//* ��ȡ��ǰQQIP���ݿ�ĸ�������
//* @param  ��
//* @return QQIP��ǰ���ݿ�ĸ�������  TDate
//*/
function TQQWry.GetQQWryDate: TDate;
var
  DateString: string;
  IPData: TStringlist;
begin
  IPData := TStringlist.Create;
  GetIPDataByIPRecordID(GetIPDataNum, IPData);
  DateString := IPData[3];
  IPData.Free;

  DateString := copy(DateString, 1, pos('IP����', DateString) - 1);
  DateString := StringReplace(DateString, '��', '-', [rfReplaceAll, rfIgnoreCase]);
  DateString := StringReplace(DateString, '��', '-', [rfReplaceAll, rfIgnoreCase]);
  DateString := StringReplace(DateString, '��', '-', [rfReplaceAll, rfIgnoreCase]);
  Result := StrToDate(DateString);
end;

///**
//* ��ȡ��ǰQQIP���ݿ����Դ��Ϣ
//* @param  ��
//* @return ��ǰQQIP���ݿ����Դ��Ϣ  string
//*/
function TQQWry.GetQQWryDataFrom: string;
var
  FromString: string;
  IPData: TStringlist;
begin
  IPData := TStringlist.Create;
  GetIPDataByIPRecordID(GetIPDataNum, IPData);
  FromString := IPData[2];
  IPData.Free;
  Result := FromString;
end;

///**
//* ����һ��IP��ַ��Ϣ��¼�ţ����ظ����¼����Ϣ
//* @param  (IPRecordID, IPData) (IP��ַ��Ϣ��¼��, ���صĸ�����Ϣ������ʼIP 15�ֽ� �ڽ���IP 15�ֽ� �۹��� �ܵ��� �ݻس���2�ֽ�)
//* @return ��
//*/
function TQQWry.GetIPLocalData(IPRecordID: int64): PChar;
var
  EndIPOffset: integer;
  i: integer;
  pBlank{, pReturn}: PChar;
  IPByteStr: string;
  IPByteStrLen: integer;
  IPDataPos: integer;
  //IPLocation: PChar;
begin
  try
    if (IPRecordID <= 0) or (IPRecordID > GetIPDataNum) then
      raise Exception.Create('IP��Ϣ��¼�Ź�С��Խ�磡');

    pBlank := '               ';
    //pReturn := #13#10;
    EndIPOffset := 0;

    //ȡ�ڴ��ļ�ӳ���׵�ַ
    pQQWryPos := pQQWryMapFile;
    //���ݼ�¼��ID���Ƶ��ü�¼�ŵ�����������Ϊ��λ�ں����ԴӺ���ǰ��
    Inc(pQQWryPos, FirstIPIndexOffset + (IPRecordID - 1) * 7 + 3);

    //ȡʼĩIP��ַ
    //������ǰ4���ֽ�Ϊ������¼����ʼIP��ַ
    IPDataPos := 0;
    for i := 0 to 3 do
    begin
      IPByteStr := IntToStr(pQQWryPos^);
      IPByteStrLen := Length(IPByteStr);
      CopyMemory(@Result[IPDataPos], PChar(IPByteStr), IPByteStrLen);
      Inc(IPDataPos, IPByteStrLen);
      if i <> 3 then
      begin
        Result[IPDataPos] := '.';
        Inc(IPDataPos);
      end;
      dec(pQQWryPos);
    end;
    //���ո���16λ
    CopyMemory(@Result[IPDataPos], pBlank, 16 - IPDataPos);
    IPDataPos := 16;

    Inc(pQQWryPos, 5);
    //��3���ֽ��Ǹ�����¼�����������ƫ��ֵ�����������ǰ4���ֽ�Ϊ������¼�Ľ���IP��ַ
    CopyMemory(@EndIPOffset, pQQWryPos, 3);

    //ȡ������¼�Ľ���IP��ַ
    pQQWryPos := pQQWryMapFile;
    Inc(pQQWryPos, EndIPOffset + 3);
    for i := 0 to 3 do
    begin
      IPByteStr := IntToStr(pQQWryPos^);
      IPByteStrLen := Length(IPByteStr);
      CopyMemory(@Result[IPDataPos], PChar(IPByteStr), IPByteStrLen);
      Inc(IPDataPos, IPByteStrLen);
      if i <> 3 then
      begin
        Result[IPDataPos] := '.';
        Inc(IPDataPos);
      end;
      dec(pQQWryPos);
    end;
    //���ո���16λ
    CopyMemory(@Result[IPDataPos], pBlank, 32 - IPDataPos);
    //IPDataPos := 32;

    //ȡ������¼�Ĺ��ҵ�����Ϣ
    //IPLocation := PChar(@Result[IPDataPos]);
    //GetIPLocationByEndIPOffset(EndIPOffset, IPLocation);

    //��β�Ļس�
   { if IPLocation[StrLen(IPLocation) - 1]<>' ' then
      Inc(IPDataPos, StrLen(IPLocation))
    else
      Inc(IPDataPos, StrLen(IPLocation) - 1);
    CopyMemory(@Result[IPDataPos], pReturn, 2);  }
  except
    on E: Exception do
    begin
      Destroy;
      raise Exception.Create(E.Message);
      exit;
    end;
  end;
end;

procedure TQQWry.GetIPDataByIPRecordID(IPRecordID: int64; var IPData: PChar);
var
  EndIPOffset: integer;
  i: integer;
  pBlank, pReturn: PChar;
  IPByteStr: string;
  IPByteStrLen: integer;
  IPDataPos: integer;
  IPLocation: PChar;
begin
  try
    if (IPRecordID <= 0) or (IPRecordID > GetIPDataNum) then
      raise Exception.Create('IP��Ϣ��¼�Ź�С��Խ�磡');

    pBlank := '               ';
    pReturn := #13#10;
    EndIPOffset := 0;

    //ȡ�ڴ��ļ�ӳ���׵�ַ
    pQQWryPos := pQQWryMapFile;
    //���ݼ�¼��ID���Ƶ��ü�¼�ŵ�����������Ϊ��λ�ں����ԴӺ���ǰ��
    Inc(pQQWryPos, FirstIPIndexOffset + (IPRecordID - 1) * 7 + 3);

    //ȡʼĩIP��ַ
    //������ǰ4���ֽ�Ϊ������¼����ʼIP��ַ
    IPDataPos := 0;
    for i := 0 to 3 do
    begin
      IPByteStr := IntToStr(pQQWryPos^);
      IPByteStrLen := Length(IPByteStr);
      CopyMemory(@IPData[IPDataPos], PChar(IPByteStr), IPByteStrLen);
      Inc(IPDataPos, IPByteStrLen);
      if i <> 3 then
      begin
        IPData[IPDataPos] := '.';
        Inc(IPDataPos);
      end;
      dec(pQQWryPos);
    end;
    //���ո���16λ
    CopyMemory(@IPData[IPDataPos], pBlank, 16 - IPDataPos);
    IPDataPos := 16;

    Inc(pQQWryPos, 5);
    //��3���ֽ��Ǹ�����¼�����������ƫ��ֵ�����������ǰ4���ֽ�Ϊ������¼�Ľ���IP��ַ
    CopyMemory(@EndIPOffset, pQQWryPos, 3);

    //ȡ������¼�Ľ���IP��ַ
    pQQWryPos := pQQWryMapFile;
    Inc(pQQWryPos, EndIPOffset + 3);
    for i := 0 to 3 do
    begin
      IPByteStr := IntToStr(pQQWryPos^);
      IPByteStrLen := Length(IPByteStr);
      CopyMemory(@IPData[IPDataPos], PChar(IPByteStr), IPByteStrLen);
      Inc(IPDataPos, IPByteStrLen);
      if i <> 3 then
      begin
        IPData[IPDataPos] := '.';
        Inc(IPDataPos);
      end;
      dec(pQQWryPos);
    end;
    //���ո���16λ
    CopyMemory(@IPData[IPDataPos], pBlank, 32 - IPDataPos);
    IPDataPos := 32;

    //ȡ������¼�Ĺ��ҵ�����Ϣ
    IPLocation := PChar(@IPData[IPDataPos]);
    GetIPLocationByEndIPOffset(EndIPOffset, IPLocation);

    //��β�Ļس�
    if IPLocation[StrLen(IPLocation) - 1] <> ' ' then
      Inc(IPDataPos, StrLen(IPLocation))
    else
      Inc(IPDataPos, StrLen(IPLocation) - 1);
    CopyMemory(@IPData[IPDataPos], pReturn, 2);
  except
    on E: Exception do
    begin
      Destroy;
      raise Exception.Create(E.Message);
      exit;
    end;
  end;
end;

///**
//* ����һ����¼�Ľ���IP��ַ��ƫ�ƣ����ظ�����¼�Ĺ��ҵ�����Ϣ
//* @param  (EndIPOffset, IPLocation) (������¼�Ľ���IP��ַƫ��, ������¼�Ĺ��ҵ�����Ϣ)
//* @return ��
//*/
procedure TQQWry.GetIPLocationByEndIPOffset(EndIPOffset: int64; var IPLocation: PChar);
const
  //ʵ����Ϣ�ִ����λ�õ��ض���ģʽ
  REDIRECT_MODE_1 = 1;
  REDIRECT_MODE_2 = 2;
var
  RedirectMode: byte;
  pSplit: PChar;
  CountryFirstOffset, CountrySecondOffset: int64;
  IPCountryLen: integer;
  IPArea: PChar;
  ///**
  //* ����һ��������Ϣƫ��ֵ�������������ļ��и�ƫ�����µĵ�����Ϣ
  //* @param  (AreaOffset, IPArea) (������Ϣ���ļ��е�ƫ��ֵ, ���صĵ�����Ϣ)
  //* @return
  //*/

  procedure ReadIPAreaByAreaOffset(AreaOffset: int64; var IPArea: PChar);
  var
    ModeByte: byte;
    ReadAreaOffset: int64;
  begin
    try
      ModeByte := 0;
      ReadAreaOffset := 0;

      //ȡ�ڴ��ļ�ӳ���׵�ַ
      pQQWryPos := pQQWryMapFile;
      //�Ƶ�ƫ�ƴ�
      inc(pQQWryPos, AreaOffset);
      //��ģʽ
      CopyMemory(@ModeByte, pQQWryPos, 1);
      //ģʽ1��2����3�ֽ�Ϊƫ��
      if (ModeByte = REDIRECT_MODE_1) or (ModeByte = REDIRECT_MODE_2) then
      begin
        //��ƫ��
        Inc(pQQWryPos);
        CopyMemory(@ReadAreaOffset, pQQWryPos, 3);
        //��ƫ��Ϊ0����Ϊδ֪������������ǰ�����ݿ����������
        if ReadAreaOffset = 0 then
          IPArea := 'δ֪����'
        else
        begin  //ȥƫ�ƴ����ַ���
          pQQWryPos := pQQWryMapFile;
          Inc(pQQWryPos, ReadAreaOffset);
          CopyMemory(IPArea, PChar(pQQWryPos), StrLen(PChar(pQQWryPos)));
        end;
      //û��ģʽ��ֱ�Ӷ��ַ���
      end
      else
      begin
        pQQWryPos := pQQWryMapFile;
        Inc(pQQWryPos, AreaOffset);
        CopyMemory(IPArea, PChar(pQQWryPos), StrLen(PChar(pQQWryPos)));
      end;
    except
      on E: Exception do
      begin
        raise Exception.Create(E.Message);
        exit;
      end;
    end;
  end;
begin
  try
    RedirectMode := 0;
    pSplit := ' ';
    CountryFirstOffset := 0;
    CountrySecondOffset := 0;

    //ȡ�ڴ��ļ�ӳ���׵�ַ
    pQQWryPos := pQQWryMapFile;
    //���ݼ�¼ID���Ƶ��ü�¼�ŵ�������
    Inc(pQQWryPos, EndIPOffset + 4);

    CopyMemory(@RedirectMode, pQQWryPos, 1);
    //�ض���ģʽ1�Ĵ���
    if RedirectMode = REDIRECT_MODE_1 then
    begin
      Inc(pQQWryPos);
      //ģʽֵΪ1�����3���ֽڵ�����Ϊ������Ϣ��ƫ��ֵ
      CopyMemory(@CountryFirstOffset, pQQWryPos, 3);
      //�����ض���
      pQQWryPos := pQQWryMapFile;
      Inc(pQQWryPos, CountryFirstOffset);
      //�ڶ��ζ�ȡ������Ϣ���ض���ģʽ
      CopyMemory(@RedirectMode, pQQWryPos, 1);
      //�ڶ����ض���ģʽΪģʽ2�Ĵ���
      if RedirectMode = REDIRECT_MODE_2 then
      begin
          //��3�ֽڵ����ݼ�Ϊ�ڶ����ض���ƫ��ֵ
        Inc(pQQWryPos);
        CopyMemory(@CountrySecondOffset, pQQWryPos, 3);
          //��ȡ�ڶ����ض���ƫ��ֵ�µ��ַ���ֵ����Ϊ������Ϣ
        pQQWryPos := pQQWryMapFile;
        Inc(pQQWryPos, CountrySecondOffset);
        IPCountryLen := StrLen(PChar(pQQWryPos));
        CopyMemory(IPLocation, PChar(pQQWryPos), IPCountryLen);
          //�ÿո�ָ���Һ͵���
        CopyMemory(@IPLocation[IPCountryLen], pSplit, 1);

          //����һ���ض���ģʽΪ1�������ض�����ȡ�ĵڶ����ض���ģʽΪ2��
          //�������Ϣ����ڵ�һ�ι�����Ϣƫ��ֵ�ĺ���
        IPArea := PChar(@IPLocation[IPCountryLen + 1]);
        ReadIPAreaByAreaOffset(CountryFirstOffset + 4, IPArea);

      //�ڶ����ض���ģʽ����ģʽ2�Ĵ���
      end
      else
      begin
        IPCountryLen := StrLen(PChar(pQQWryPos));
        CopyMemory(IPLocation, PChar(pQQWryPos), IPCountryLen);
          //�ÿո�ָ���Һ͵���
        CopyMemory(@IPLocation[IPCountryLen], pSplit, 1);
          //��������Ϣ
        IPArea := PChar(@IPLocation[IPCountryLen + 1]);
        ReadIPAreaByAreaOffset(CountryFirstOffset + IPCountryLen + 1, IPArea);
      end;

    //�ض���ģʽ2�Ĵ���
    end
    else if RedirectMode = REDIRECT_MODE_2 then
    begin
      Inc(pQQWryPos);
      //ģʽֵΪ2�����3���ֽڵ�����Ϊ������Ϣ��ƫ��ֵ
      CopyMemory(@CountrySecondOffset, pQQWryPos, 3);
      //�����ض���
      pQQWryPos := pQQWryMapFile;
      Inc(pQQWryPos, CountrySecondOffset);
      //������Ϣ
      IPCountryLen := StrLen(PChar(pQQWryPos));
      CopyMemory(IPLocation, PChar(pQQWryPos), IPCountryLen);
      //�ÿո�ָ���Һ͵���
      CopyMemory(@IPLocation[IPCountryLen], pSplit, 1);

      //������Ϣ
      IPArea := PChar(@IPLocation[IPCountryLen + 1]);
      ReadIPAreaByAreaOffset(EndIPOffset + 8, IPArea);
    //�����ض���ģʽ�Ĵ�����ŵļ���IP��ַ��Ϣ
    end
    else
    begin
      //������Ϣ
      IPCountryLen := StrLen(PChar(pQQWryPos));
      CopyMemory(IPLocation, PChar(pQQWryPos), IPCountryLen);
      //�ÿո�ָ���Һ͵���
      CopyMemory(@IPLocation[IPCountryLen], pSplit, 1);

      //������Ϣ
      IPArea := PChar(@IPLocation[IPCountryLen + 1]);
      ReadIPAreaByAreaOffset(EndIPOffset + 4 + IPCountryLen + 1, IPArea);
    end;
  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message);
      exit;
    end;
  end;
end;

///**
//* ����һ��IP��ַ��Ϣ��¼�ţ����ظ����¼����Ϣ����Stringlist���ո�����Ϣ��Ч�ʽϵ�
//* @param  (IPRecordID, IPData) (IP��ַ��Ϣ��¼��, ���صĸ�����Ϣ������ʼIP �ڽ���IP �۹��� �ܵ���)
//* @return ��
//*/
procedure TQQWry.GetIPDataByIPRecordID(IPRecordID: int64; var IPData: TStringlist);
var
  aryIPData: array[0..254] of char;
  pIPData: PChar;
  i: integer;
begin
  try
    FillChar(aryIPData, SizeOf(aryIPData), #0);
    pIPData := PChar(@aryIPData[0]);

    GetIPDataByIPRecordID(IPRecordID, pIPData);
    //ȥ����β�Ļس���
    pIPData[StrLen(pIPData) - 2] := #0;
    IPData.CommaText := StrPas(pIPData);
    //�п��ܵ���Ϊ�գ�Ҳ�п��ܵ����к��пո�
    for i := 1 to 4 - IPData.Count do
      IPData.Add('��');
    for i := 5 to IPData.Count do
      IPData[3] := IPData[3] + ' ' + IPData[i - 1];
  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message);
      exit;
    end;
  end;
end;

///**
//* ����һ��IP��ַ���Ķε���ַ�����ʽ�������ظ�IP����ֵ
//* @param  (IP)  (IP��ַ���Ķε���ַ�����ʽ��
//* @return ��IP����ֵ
//*/
function TQQWry.GetIPValue(IP: string): int64;
var
  slIP: TStringlist;
  i: integer;
  function SplitStringToStringlist(aString: string; aSplitChar: string): TStringlist;
  begin
    Result := TStringList.Create;
    while pos(aSplitChar, aString) > 0 do
    begin
      Result.Add(copy(aString, 1, pos(aSplitChar, aString) - 1));
      aString := copy(aString, pos(aSplitChar, aString) + 1, length(aString) - pos(aSplitChar, aString));
    end;
    Result.Add(aString);
  end;
begin
  try
    slIP := SplitStringToStringlist(IP, '.');
    Result := 0;
    for i := 3 downto 0 do
    begin
      Result := Result + StrToInt(slIP[i]) * trunc(power(256, 3 - i));
    end;
  except
    on E: Exception do
    begin
      raise Exception.Create('��Ч��IP��ַ��');
      exit;
    end;
  end;
end;

///**
//* ����һ��IP��ַ���Ķε���ַ�����ʽ�������ظ�IP��ַ���ڵļ�¼��
//* @param  IP  IP��ַ���Ķε���ַ�����ʽ��  string
//* @return ��IP��ַ���ڵļ�¼��  Cardinal
//*/
function TQQWry.GetIPDataID(IP: string): int64;

  function SearchIPDataID(IPRecordFrom, IPRecordTo, IPValue: int64): int64;
  var
    CompareIPValue1, CompareIPValue2: int64;
  begin
    Result := 0;
    CompareIPValue1 := 0;
    CompareIPValue2 := 0;

    pQQWryPos := pQQWryMapFile;
    Inc(pQQWryPos, FirstIPIndexOffset + ((IPRecordTo - IPRecordFrom) div 2 + IPRecordFrom - 1) * 7);
    CopyMemory(@CompareIPValue1, pQQWryPos, 4);
    pQQWryPos := pQQWryMapFile;
    Inc(pQQWryPos, FirstIPIndexOffset + ((IPRecordTo - IPRecordFrom) div 2 + IPRecordFrom) * 7);
    CopyMemory(@CompareIPValue2, pQQWryPos, 4);
    //�ҵ���
    if (IPRecordFrom = IPRecordTo) or ((IPValue >= CompareIPValue1) and (IPValue < CompareIPValue2)) then
    begin
      Result := (IPRecordTo - IPRecordFrom) div 2 + IPRecordFrom;
    end else
    if IPValue > CompareIPValue1 then //������
    begin
      Result := SearchIPDataID((IPRecordTo - IPRecordFrom) div 2 + IPRecordFrom + 1, IPRecordTo, IPValue);
    end
    else
    if IPValue < CompareIPValue1 then   //ǰ�����
    begin
      Result := SearchIPDataID(IPRecordFrom, (IPRecordTo - IPRecordFrom) div 2 + IPRecordFrom - 1, IPValue);
    end;
  end;

begin
  try
    Result := SearchIPDataID(1, GetIPDataNum, GetIPValue(IP));
  except
    on E: Exception do
    begin
      Destroy;
      raise Exception.Create(E.Message);
      exit;
    end;
  end;
end;

///**
//* ��IP��ַ���ݿ��ѹ���ı��ļ�
//* @param  (ATxtFileName) (��ѹ����ı��ļ�ȫ��)
//* @return -1Ϊ��ѹʧ�ܣ���-1ֵΪ��ѹ����ʱ�䣬��λ����
//*/
function TQQWry.ExtractIPDataToTxtFile(ATxtFileName: string): integer;
var
  QQWryMemoryStream: TMemoryStream;
  i: integer;
  IPData, NowPos: PChar;
  TimeCounter: DWORD;
  pReturn: PChar;
begin
  //result := -1;
  try
    IPData := StrAlloc(41943040);
    NowPos := IPData;

    TimeCounter := GetTickCount;
    for i := 1 to GetIPDataNum do
    begin
      GetIPDataByIPRecordID(i, NowPos);
      Inc(NowPos, StrLen(NowPos));
    end;
    pReturn := #13#10;
    NowPos := StrECopy(NowPos, pReturn);
    NowPos := StrECopy(NowPos, pReturn);
    NowPos := StrECopy(NowPos, PChar(format('IP���ݿ⹲������ �� %d ��', [GetIPDataNum])));
    NowPos := StrECopy(NowPos, pReturn);

    QQWryMemoryStream := TMemoryStream.Create;
    QQWryMemoryStream.SetSize(NowPos - IPData);
    QQWryMemoryStream.WriteBuffer(IPData^, NowPos - IPData);
    QQWryMemoryStream.SaveToFile(ATxtFileName);
    StrDispose(IPData);
    QQWryMemoryStream.Destroy;
    result := GetTickCount - TimeCounter;
  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message);
      exit;
    end;
  end;
end;

end.

//unit QQWry;
//
//interface
//
//uses
//  System.Classes, System.SysUtils,
//  Winapi.WinSock, Vcl.Forms,
//  System.Math, System.SyncObjs;
//
//type
//  TQQWry = class(TObject)
//  private
//    QQWryFileName: string;
//    QQWryFileStream: TBufferedFileStream;
//    QQWryFileSize: Cardinal;
//    IPRecordNum: Cardinal;
//    FirstIPIndexOffset, LastIPIndexOffset: Cardinal;
//    FLock: TCriticalSection;
//    function GetQQWryFileName: string;
//    function GetQQWryFileSize: Cardinal;
//    function GetIPRecordNum: Cardinal;
//    function GetQQWryDate: TDate;
//    function GetQQWryDataFrom: string;
//    function GetIPLocation(IPLocationOffset: Cardinal): TStringlist;
//    function GetIPMsg(IPRecordID: Cardinal): TStringlist;
//    function GetIPRecordID(IP: string): Cardinal;
//    function GetIPValue(IP: string): Cardinal;
//  public
//    constructor Create(cQQWryFileName: string);
//    destructor Destroy; override;
//    function GetLocation(IP: string): string;
//  end;
//
//function IPLocation: TQQWry;
//
//implementation
//
//var
//  __IPLocation: TQQWry;
//
//function IPLocation: TQQWry;
//begin
//  if __IPLocation = nil then
//    __IPLocation := TQQWry.Create(ExtractFilePath(ParamStr(0)) + 'qqwry.dat');
//
//  Result := __IPLocation;
//end;
//
//{
//TQQWry
//}
//
//constructor TQQWry.Create(cQQWryFileName: string);
//begin
//  inherited Create;
//  FLock := TCriticalSection.Create;
//  QQWryFileName := cQQWryFileName;
//  QQWryFileStream := TBufferedFileStream.Create(QQWryFileName, fmOpenRead or fmShareDenyWrite, 0);
//  QQWryFileSize := QQWryFileStream.Size;
//  QQWryFileStream.Read(FirstIPIndexOffset, 4);
//  QQWryFileStream.Read(LastIPIndexOffset, 4);
//  IPRecordNum := (LastIPIndexOffset - FirstIPIndexOffset) div 7 + 1;
//end;
//
//destructor TQQWry.Destroy;
//begin
//  QQWryFileStream.Free;
//  FLock.Free;
//  inherited Destroy;
//end;
//
//function TQQWry.GetIPLocation(IPLocationOffset: Cardinal): TStringlist;
//const
//  // ʵ����Ϣ�ִ����λ�õ��ض���ģʽ
//  REDIRECT_MODE_1 = 1;
//  REDIRECT_MODE_2 = 2;
//var
//  RedirectMode: byte;
//  CountryFirstOffset, CountrySecondOffset: Cardinal;
//  CountryMsg, AreaMsg: string;
//  //
//  function ReadString(StringOffset: Cardinal): ansistring;
//  var
//    ReadByte: ansichar;
//  begin
//    Result := '';
//    QQWryFileStream.Seek(StringOffset, soFromBeginning);
//    QQWryFileStream.Read(ReadByte, 1);
//    while ord(ReadByte) <> 0 do
//    begin
//      Result := Result + ReadByte;
//      QQWryFileStream.Read(ReadByte, 1);
//
//    end;
//  end;
////
//  function ReadArea(AreaOffset: Cardinal): ansistring;
//  var
//    ModeByte: byte;
//    ReadAreaOffset: Cardinal;
//  begin
//    ReadAreaOffset := 0;
//    QQWryFileStream.Seek(AreaOffset, soFromBeginning);
//    QQWryFileStream.Read(ModeByte, 1);
//    if (ModeByte = REDIRECT_MODE_1) or (ModeByte = REDIRECT_MODE_2) then
//    begin
//      QQWryFileStream.Read(ReadAreaOffset, 3);
//
//      if ReadAreaOffset = 0 then
//        Result := 'δ֪����'
//      else
//        Result := ReadString(ReadAreaOffset);
//    end
//    else
//    begin
//      Result := ReadString(AreaOffset);
//    end;
//  end;
//begin
//  CountryFirstOffset := 0;
//  CountrySecondOffset := 0;
//  // ����4���ֽڣ���4�ֽ�����Ϊ����IP��Ϣ��IP��ַ���е���ֹIPֵ
//  QQWryFileStream.Seek(IPLocationOffset + 4, soFromBeginning);
//  //��ȡ������Ϣ���ض���ģʽֵ
//  QQWryFileStream.Read(RedirectMode, 1);
//  //�ض���ģʽ1�Ĵ���
//  if RedirectMode = REDIRECT_MODE_1 then
//  begin
//    //ģʽֵΪ1�����3���ֽڵ�����Ϊ������Ϣ���ض���ƫ��ֵ
//    QQWryFileStream.ReadData(CountryFirstOffset, 3);
//    //�����ض���
//    QQWryFileStream.Seek(CountryFirstOffset, soFromBeginning);
//    //�ڶ��ζ�ȡ������Ϣ���ض���ģʽ
//    QQWryFileStream.Read(RedirectMode, 1);
//    //�ڶ����ض���ģʽΪģʽ2�Ĵ���
//    if RedirectMode = REDIRECT_MODE_2 then
//    begin
//      //��3�ֽڵ����ݼ�Ϊ�ڶ����ض���ƫ��ֵ
//      QQWryFileStream.ReadData(CountrySecondOffset, 3);
//      //��ȡ�ڶ����ض���ƫ��ֵ�µ��ַ���ֵ����Ϊ������Ϣ
//      CountryMsg := ReadString(CountrySecondOffset);
//      //����һ���ض���ģʽΪ1�������ض�����ȡ�ĵڶ����ض���ģʽΪ2��
//      //�������Ϣ����ڵ�һ�ι�����Ϣƫ��ֵ�ĺ���
//      QQWryFileStream.Seek(CountryFirstOffset + 4, soFromBeginning);
//      //�ڶ����ض���ģʽ����ģʽ2�Ĵ���
//    end
//    else
//    begin
//      CountryMsg := ReadString(CountryFirstOffset);
//    end;
//    //���ض���ģʽ1�¶�������Ϣֵ
//    AreaMsg := ReadArea(QQWryFileStream.Position);
//    //�ض���ģʽ2�Ĵ���
//  end else
//  if RedirectMode = REDIRECT_MODE_2 then
//  begin
//    QQWryFileStream.ReadData(CountrySecondOffset, 3);
//    CountryMsg := ReadString(CountrySecondOffset);
//    AreaMsg := ReadArea(IPLocationOffset + 8);
//    //�����ض���ģʽ�Ĵ�����ŵļ���IP��ַ��Ϣ
//  end else
//  begin
//    CountryMsg := ReadString(QQWryFileStream.Position - 1);
//    AreaMsg := ReadArea(QQWryFileStream.Position);
//  end;
//  Result := TStringlist.Create;
//  Result.Add(CountryMsg);
//  Result.Add(AreaMsg);
//end;
//
//function TQQWry.GetIPMsg(IPRecordID: Cardinal): TStringlist;
//var
//  aryStartIP: array[1..4] of byte;
//  strStartIP: string;
//  EndIPOffset: Cardinal;
//  aryEndIP: array[1..4] of byte;
//  strEndIP: string;
//  i: integer;
//begin
//  EndIPOffset := 0;
//  //���ݼ�¼ID���Ƶ��ü�¼�ŵ�������
//  QQWryFileStream.Seek(FirstIPIndexOffset + (IPRecordID - 1) * 7, soFromBeginning);
//  //������ǰ4���ֽ�Ϊ��ʼIP��ַ
//  QQWryFileStream.Read(aryStartIP, 4);
//  //��3���ֽ������������ƫ��ֵ
//  //
//  QQWryFileStream.Read(EndIPOffset, 3);
//  QQWryFileStream.ReadData(EndIPOffset, 3);
//  // ������������
//  QQWryFileStream.Seek(EndIPOffset, soFromBeginning);
//  //���������ǰ4���ֽ�Ϊ��ֹIP��ַ
//  QQWryFileStream.Read(aryEndIP, 4);
//  //����ֹIP��ַת��Ϊ��ֵ���ʽ
//  strStartIP := '';
//  for i := 4 downto 1 do
//  begin
//    if i <> 1 then
//      strStartIP := strStartIP + IntToStr(aryStartIP[i]) + '.'
//    else
//      strStartIP := strStartIP + IntToStr(aryStartIP[i]);
//  end;
//
//  strEndIP := '';
//  for i := 4 downto 1 do
//  begin
//    if i <> 1 then
//      strEndIP := strEndIP + IntToStr(aryEndIP[i]) + '.'
//    else
//      strEndIP := strEndIP + IntToStr(aryEndIP[i]);
//  end;
//  Result := TStringlist.Create;
//  Result.Add(strStartIP);
//  Result.Add(strEndIP);
//  // ��ȡ������¼�µ�IP��ַ��Ϣ
//  //����������ͳһ�ģ������������ƫ��ֵ ����ֹIP��ַ�Ĵ��λ�� �۹�����Ϣ��������ֹIP��ַ���λ�ú�
//  Result.AddStrings(GetIPLocation(EndIPOffset));
//end;
//
//function TQQWry.GetIPRecordID(IP: string): Cardinal;
//  function SearchIPRecordID(IPRecordFrom, IPRecordTo, IPValue: Cardinal): Cardinal;
//  var
//    CompareIPValue1, CompareIPValue2: Cardinal;
//  begin
//    Result := 0;
//    CompareIPValue1 := 0;
//    CompareIPValue2 := 0;
//    QQWryFileStream.Seek(FirstIPIndexOffset + ((IPRecordTo - IPRecordFrom) div 2 + IPRecordFrom - 1) * 7, soFromBeginning);
//    QQWryFileStream.Read(CompareIPValue1, 4);
//    QQWryFileStream.Seek(FirstIPIndexOffset + ((IPRecordTo - IPRecordFrom) div 2 + IPRecordFrom) * 7, soFromBeginning);
//    QQWryFileStream.Read(CompareIPValue2, 4);
//    //�ҵ���
//    if (IPValue >= CompareIPValue1) and (IPValue < CompareIPValue2) then
//    begin
//      Result := (IPRecordTo - IPRecordFrom) div 2 + IPRecordFrom;
//    end else
//    if IPValue > CompareIPValue1 then  //������
//    begin
//      Result := SearchIPRecordID((IPRecordTo - IPRecordFrom) div 2 + IPRecordFrom + 1, IPRecordTo, IPValue);
//    end else
//    if IPValue < CompareIPValue1 then   //ǰ�����
//    begin
//      Result := SearchIPRecordID(IPRecordFrom, (IPRecordTo - IPRecordFrom) div 2 + IPRecordFrom - 1, IPValue);
//    end;
//  end;
//begin
//  Result := SearchIPRecordID(1, GetIPRecordNum, GetIPValue(IP));
//end;
//
//function TQQWry.GetIPRecordNum: Cardinal;
//begin
//  Result := IPRecordNum;
//end;
//
//function TQQWry.GetIPValue(IP: string): Cardinal;
//var
//  tsIP: TStringlist;
//  i: integer;
//  function SplitStringToStringlist(aString: string; aSplitChar: string): TStringlist;
//  begin
//    Result := TStringlist.Create;
//    while pos(aSplitChar, aString) > 0 do
//    begin
//      Result.Add(copy(aString, 1, pos(aSplitChar, aString) - 1));
//      aString := copy(aString, pos(aSplitChar, aString) + 1, length(aString) - pos(aSplitChar, aString));
//    end;
//    Result.Add(aString);
//  end;
//begin
//  tsIP := SplitStringToStringlist(IP, '.');
//  Result := 0;
//  for i := 3 downto 0 do
//  begin
//    Result := Result + StrToInt(tsIP[i]) * trunc(power(256, 3 - i));
//  end;
//
//end;
//
//function TQQWry.GetLocation(IP: string): string;
//begin
//  FLock.Enter;
//  try
//    Result := GetIPMsg(GetIPRecordID(IP))[2];
//  finally
//    FLock.Leave;
//  end;
//end;
//
//function TQQWry.GetQQWryDataFrom: string;
//begin
//  Result := GetIPMsg(GetIPRecordNum)[2];
//end;
//
//function TQQWry.GetQQWryDate: TDate;
//var
//  DateString: string;
//begin
//  DateString := GetIPMsg(GetIPRecordNum)[3];
//  DateString := copy(DateString, 1, pos('IP����', DateString) - 1);
//  DateString := StringReplace(DateString, '��', '-', [rfReplaceAll, rfIgnoreCase]);
//  DateString := StringReplace(DateString, '��', '-', [rfReplaceAll, rfIgnoreCase]);
//  DateString := StringReplace(DateString, '��', '-', [rfReplaceAll, rfIgnoreCase]);
//  Result := StrToDate(DateString);
//end;
//
//function TQQWry.GetQQWryFileName: string;
//begin
//  Result := QQWryFileName;
//end;
//
//function TQQWry.GetQQWryFileSize: Cardinal;
//begin
//  Result := QQWryFileSize;
//end;
//
//initialization
//
//finalization
//  if __IPLocation <> nil then
//    __IPLocation.Free;
//
//end.


