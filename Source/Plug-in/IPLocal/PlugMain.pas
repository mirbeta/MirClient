//=============================================================================
//���ú���˵��:
//   �������ֵ����������̨��:
//     procedure MainOutMessasge(sMsg:String;nMode:integer)
//     sMsg ΪҪ���͵��ı�����
//     nMode Ϊ����ģʽ��0Ϊ�����ڿ���̨����ʾ��1Ϊ������ʾ���У��Ժ���ʾ
//
//   ȡ��0-255���������ɫ
//     function GetRGB(bt256:Byte):TColor;
//     bt256 Ҫ��ѯ����
//     ����ֵ Ϊ�������ɫ
//
//   ���͹㲥���֣�
//     procedure SendBroadCastMsg(sMsg:String;MsgType:TMsgType);
//     sMsg Ҫ���͵�����
//     MsgType ��������
//=============================================================================
unit PlugMain;

interface

uses
  Windows, SysUtils, StrUtils, Classes;

procedure InitPlug(AppHandle: THandle);

procedure UnInitPlug();

function DeCodeText(sText: string): string;

function SearchIPLocal(sIPaddr: string): string;

implementation

uses
  Module, QQWry, Share;
//=============================================================================
//���ز��ģ��ʱ���õĳ�ʼ������
//������Apphandle Ϊ��������
//=============================================================================

procedure InitPlug(AppHandle: THandle);
begin
  MainOutMessasge(sStartLoadPlug, 0);
end;
//=============================================================================
//�˳����ģ��ʱ���õĽ�������
//=============================================================================

procedure UnInitPlug();
begin
  {
    д����Ӧ�������;
  }
  MainOutMessasge(sUnLoadPlug, 0);
end;
//=============================================================================
//��Ϸ��־��Ϣ������
//����ֵ��True ��������Ĭ����Ϸ��־��������False ����Ĭ����Ϸ��־������
//=============================================================================

function GameDataLog(sLogMsg: string): Boolean;
begin
  {
    д����Ӧ������Ϸ��־����;
  }
  Result := False;
end;

//=============================================================================
//��Ϸ�ı�������Ϣ���뺯��(һ�����ڼӽ��ܽű�)
//������sText ΪҪ������ַ���
//����ֵ�����ؽ������ַ���(���ص��ַ������Ȳ��ܳ���1024�ֽڣ��������������)
//=============================================================================
function DeCodeText(sText: string): string;
begin
  {try
    if (sText<>'') and (sText[1]<>';') then
      Result:=DecryStrHex(sText,sKey);
  except
    Result:='';
  end;}
  //Result:='����ֵ�����ؽ������ַ���';
end;

function DecryStrHex(StrHex: string): string;
//UniCode -> ����

  function UniCode2Chinese(AiUniCode: Integer): string;
  var
    ch, cl: string[3];
    s: string;
  begin
    s := IntToHex(AiUniCode, 2);
    cl := '$' + Copy(s, 1, 2);
    ch := '$' + Copy(s, 3, 2);
    s := Chr(StrToInt(ch)) + Chr(StrToInt(cl)) + #0;
    Result := WideCharToString(pWideChar(WideString(s)));
  end;

var
  nLength: Integer;
  I: Integer;
  Hexstr: string;
  nAm: Integer;
begin
  Result := '';
  try
    I := 1;
    nLength := Length(StrHex);
    while I <= nLength do
    begin
      Hexstr := Copy(StrHex, I, 4);
      nAm := StrToInt('$' + Hexstr);
      if nAm < 128 then
      begin
        Result := Result + Chr(nAm);
      end
      else if nAm > 127 then
      begin
        Result := Result + UniCode2Chinese(nAm);
      end;
      Inc(I, 4);
    end;
  except
  end;
end;

function EncryStrHex(StrHex: string): string;
//���� -> UniCode

  function Chinese2UniCode(AiChinese: string): Integer;
  var
    ch, cl: string[2];
    a: array[1..2] of char;
  begin
    StringToWideChar(Copy(AiChinese, 1, 2), @(a[1]), 2);
    ch := IntToHex(Integer(a[2]), 2);
    cl := IntToHex(Integer(a[1]), 2);
    Result := StrToInt('$' + ch + cl);
  end;

var
  nLength: Integer;
  I: Integer;
  Hexstr: string;
begin
  Result := '';
  try
    I := 1;
    nLength := Length(StrHex);
    while I <= nLength do
    begin
      if (ByteType(StrHex, I) = mbSingleByte) and (StrHex <> '') then
      begin
        Hexstr := MidStr(WideString(StrHex), I, 1);
        if Hexstr <> '' then
          Result := Result + IntToHex(Chinese2UniCode(Hexstr), 4);
      end
      else if ((ByteType(StrHex, I) = mbLeadByte) or (ByteType(StrHex, I) = mbTrailByte)) and (StrHex <> '') then
      begin
        Hexstr := MidStr(WideString(StrHex), I, 1);
        if Hexstr <> '' then
          Result := Result + IntToHex(Chinese2UniCode(Hexstr), 4);
      end;
      Inc(I);
    end;
  except
  end;
end;

//=============================================================================
//IP���ڵز�ѯ����
//������sIPaddr ΪҪ��ѯ��IP��ַ
//����ֵ������IP���ڵ��ı���Ϣ(���ص��ַ������Ȳ��ܳ���255�ֽڣ������ᱻ�ض�)
//=============================================================================

function SearchIPLocal(sIPaddr: string): string;
var
  sLOCAL: string;
begin
  try
    sLOCAL := g_TQQWry.GetLocationByIpAddress(sIPaddr);
    Result := Trim(sLOCAL);
  except
    Result := '';
  end;
end;

end.

