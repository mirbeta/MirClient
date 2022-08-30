{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2010 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnInetUtils;
{* |<PRE>
================================================================================
* ������ƣ�����ͨѶ�����
* ��Ԫ���ƣ�ʹWinInet ��װ��Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע�������� TCnHTTP��ʹ�� WinInet ����ȡ HTTP ����
* ����ƽ̨��PWin2000Pro + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* ��Ԫ��ʶ��$Id: CnInetUtils.pas 557 2010-05-01 09:32:54Z zhoujingyu $
* �޸ļ�¼��2005.09.14 V1.1
*                ���� UserAgent �� Proxy ����(�� illk �ṩ)
*           2003.03.09 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, WinInet;

type

//==============================================================================
// ʹ�� WinInet ��ȡ HTTP �ļ�����
//==============================================================================

{ TCnInet }

  TCnInetProgressEvent = procedure (Sender: TObject; TotalSize, CurrSize: Integer;
    var Abort: Boolean) of object;
  {* �������ؽ����¼�
   |<PRE>
     Sender     - �̶߳���
     TotalSize  - ���ֽ��������Ϊ -1����ʾ����δ֪
     CurrSize   - ��ǰ����ֽ���
     Abort      - �Ƿ��ж�
   |</PRE>}

  TCnURLInfo = record
    Protocol: string;
    Host: string;
    Port: string;
    PathName: string;
    Username: string;
    Password: string;
  end;

  TCnInet = class
  {* ʹ�� WinInet ��ȡ HTTP/FTP �ļ����ࡣ}
  private
    hSession: HINTERNET;
    FAborted: Boolean;
    FGetDataFail: Boolean;
    FOnProgress: TCnInetProgressEvent;
    FUserAgent:string;
    FProxyServer:string;
    FProxyUserName:string;
    FProxyPassWord:string;
    FHttpRequestHeaders: TStringList;
    function ParseURL(URL: string; var Info: TCnURLInfo): Boolean;
  protected
    procedure DoProgress(TotalSize, CurrSize: Integer);
    function InitInet: Boolean;
    procedure CloseInet;
    function GetStreamFromHandle(Handle: HINTERNET; TotalSize: Integer;
      Stream: TStream): Boolean;
    function GetHTTPStream(Info: TCnURLInfo; Stream: TStream): Boolean;
    function GetFTPStream(Info: TCnURLInfo; Stream: TStream): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Abort;
    {* �жϵ�ǰ����}
    function GetStream(const AURL: string; Stream: TStream): Boolean;
    {* �� AURL ��ַ��ȡ���ݵ��� Stream}
    function GetString(const AURL: string): AnsiString;
    {* �� AURL ��ַ����һ���ַ���}
    function GetFile(const AURL, FileName: string): Boolean;
    {* �� AURL ��ַ��ȡ���ݱ��浽�ļ� FileName}
    property OnProgress: TCnInetProgressEvent read FOnProgress write FOnProgress;
    {* ���ݽ����¼�}
    property Aborted: Boolean read FAborted;
    {* �Ƿ��ѱ��ж�}
    property GetDataFail: Boolean read FGetDataFail;
    {* ��һ�ε����ݶ�ȡ�Ƿ�ɹ�}

    property UserAgent: string read FUserAgent write FUserAgent;
    {* ����UserAgent �����ʶ���ʾ}
    property ProxyServer: string read FProxyServer write FProxyServer;
    {* �������������: [Э��=][Э��://]������[:�˿�] �� 127.0.0.1:8080}
    property ProxyUserName: string read FProxyUserName write FProxyUserName;
    {* ����������û���}
    property ProxyPassWord: string read FProxyPassWord write FProxyPassWord;
    {* ����������û�����}
    property HttpRequestHeaders: TStringList read FHttpRequestHeaders;
    {* ������Ϣͷ}
  end;

  TCnHTTP = class(TCnInet);

  TCnFTP = class(TCnInet);

function EncodeURL(const URL: string): string;
{* �� URL �е������ַ�ת���� %XX ����ʽ}

function CnInet_GetStream(const AURL: string; Stream: TStream): Boolean;
function CnInet_GetString(const AURL: string): AnsiString;
function CnInet_GetFile(const AURL, FileName: string): Boolean;

implementation

const
  csBufferSize = 4096;

function EncodeURL(const URL: string): string;
const
  UnsafeChars = ['*', '#', '%', '<', '>', '+', ' '];
var
  i: Integer;
  InStr, OutStr: AnsiString;
begin
  InStr := AnsiString(URL);
  OutStr := '';
  for i := 1 to Length(InStr) do begin
    if (InStr[i] in UnsafeChars) or (InStr[i] >= #$80) or (InStr[i] < #32) then
      OutStr := OutStr + '%' + AnsiString(IntToHex(Ord(InStr[i]), 2))
    else
      OutStr := OutStr + InStr[i];
  end;
  Result := string(OutStr);
end;

function CnInet_GetStream(const AURL: string; Stream: TStream): Boolean;
begin
  with TCnInet.Create do
  try
    Result := GetStream(AURL, Stream);
  finally
    Free;
  end;
end;

function CnInet_GetString(const AURL: string): AnsiString;
begin
  with TCnInet.Create do
  try
    Result := GetString(AURL);
  finally
    Free;
  end;
end;

function CnInet_GetFile(const AURL, FileName: string): Boolean;
begin
  with TCnInet.Create do
  try
    Result := GetFile(AURL, FileName);
  finally
    Free;
  end;
end;

//==============================================================================
// ʹ�� WinInet ��ȡ HTTP �ļ�����
//==============================================================================

{ TCnInet }

constructor TCnInet.Create;
begin
  inherited;
  FUserAgent := 'CnPack Internet Utils';
  FHttpRequestHeaders := TStringList.Create;
end;

destructor TCnInet.Destroy;
begin
  CloseInet;
  FHttpRequestHeaders.Free;
  inherited;
end;

procedure TCnInet.CloseInet;
begin
  if hSession <> nil then
  begin
    InternetCloseHandle(hSession);
    hSession := nil;
  end;
end;

function TCnInet.InitInet: Boolean;
begin
  if hSession = nil then
  begin
    if Length(FProxyServer) = 0 then
    begin
      hSession := InternetOpen(PChar(FUserAgent), INTERNET_OPEN_TYPE_PRECONFIG,
        nil, nil, 0);
    end
    else
    begin
      hSession := InternetOpen(PChar(FUserAgent), INTERNET_OPEN_TYPE_PROXY,
        PChar(FProxyServer), nil, 0);
      if Length(FProxyUserName) > 0 then
        InternetSetOption(hSession, INTERNET_OPTION_PROXY_USERNAME, PChar(FProxyUserName), Length(FProxyUserName));
      if Length(FProxyPassWord) > 0 then
        InternetSetOption(hSession, INTERNET_OPTION_PROXY_PASSWORD, PChar(FProxyPassWord), Length(FProxyPassWord));
    end;
  end;
  Result := hSession <> nil;
end;

procedure TCnInet.Abort;
begin
  FAborted := True;
end;

procedure TCnInet.DoProgress(TotalSize, CurrSize: Integer);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, TotalSize, CurrSize, FAborted);
end;

function TCnInet.ParseURL(URL: string; var Info: TCnURLInfo): Boolean;
var
  Idx: Integer;
  Buff: string;
  
  function ExtractStr(var ASrc: string; ADelim: string;
    ADelete: Boolean = True): string;
  var
    Idx: Integer;
  begin
    Idx := Pos(ADelim, ASrc);
    if Idx = 0 then
    begin
      Result := ASrc;
      if ADelete then
        ASrc := '';
    end
    else
    begin
      Result := Copy(ASrc, 1, Idx - 1);
      if ADelete then
        ASrc := Copy(ASrc, Idx + Length(ADelim), MaxInt);
    end;
  end;
begin
  Result := False;
  URL := Trim(URL);
  Idx := Pos('://', URL);
  if Idx > 0 then
  begin
    Info.Protocol := Copy(URL, 1, Idx  - 1);
    Delete(URL, 1, Idx + 2);
    if URL = '' then Exit;

    Buff := ExtractStr(URL, '/');
    Idx := Pos('@', Buff);
    Info.Password := Copy(Buff, 1, Idx  - 1);
    if Idx > 0 then Delete(Buff, 1, Idx);

    Info.UserName := ExtractStr(Info.Password, ':');
    if Length(Info.UserName) = 0 then
      Info.Password := '';

    Info.Host := ExtractStr(Buff, ':');
    Info.Port := Buff;
    Info.PathName := URL;
    Result := True;
  end;
end;

function TCnInet.GetStream(const AURL: string; Stream: TStream): Boolean;
var
  Info: TCnURLInfo;
begin
  Result := False;
  if not ParseURL(AURL, Info) then
    Exit;

  FAborted := False;
  if not InitInet or FAborted then
    Exit;

  if SameText(Info.Protocol, 'http') or SameText(Info.Protocol, 'https') then
    Result := GetHTTPStream(Info, Stream)
  else if SameText(Info.Protocol, 'ftp') then
    Result := GetFTPStream(Info, Stream);

  if FAborted then
    Result := False;
    
  FGetDataFail := not Result;
end;

function TCnInet.GetStreamFromHandle(Handle: HINTERNET; TotalSize: Integer;
  Stream: TStream): Boolean;
var
  CurrSize, Readed: DWORD;
  Buf: array[0..csBufferSize - 1] of Byte;
begin
  Result := False;
  CurrSize := 0;
  Readed := 0;
  repeat
    if not InternetReadFile(Handle, @Buf, csBufferSize, Readed) then
      Exit;
    if Readed > 0 then
    begin
      Stream.Write(Buf, Readed);
      Inc(CurrSize, Readed);
      DoProgress(TotalSize, CurrSize);
      if Aborted then Exit;
    end;
  until Readed = 0;
  Result := True;
end;

function TCnInet.GetFTPStream(Info: TCnURLInfo; Stream: TStream): Boolean;
var
  hConnect, hFtp: HINTERNET;
  FindData: TWin32FindData;
  TotalSize: Integer;
begin
  Result := False;
  hConnect := nil;
  hFtp := nil;
  try
    hConnect := InternetConnect(hSession, PChar(Info.Host),
      StrToIntDef(Info.Port, INTERNET_DEFAULT_FTP_PORT),
      PChar(Info.Username), PChar(Info.Password),
      INTERNET_SERVICE_FTP, 0, 0);
    if (hConnect = nil) or FAborted then
      Exit;

    hFtp := FtpFindFirstFile(hConnect, PChar(Info.PathName), FindData,
      INTERNET_FLAG_NEED_FILE, 0);
    if hFtp <> nil then
    begin
      InternetCloseHandle(hFtp);
      TotalSize := FindData.nFileSizeLow;
    end
    else
      TotalSize := -1;

    hFtp := FtpOpenFile(hConnect, PChar(Info.PathName), GENERIC_READ,
      FTP_TRANSFER_TYPE_BINARY, 0);
    if (hFtp = nil) or FAborted then
      Exit;
      
    Result := GetStreamFromHandle(hFtp, TotalSize, Stream);
  finally
    if hFtp <> nil then InternetCloseHandle(hFtp);
    if hConnect <> nil then InternetCloseHandle(hConnect);
  end;
end;

function TCnInet.GetHTTPStream(Info: TCnURLInfo; Stream: TStream): Boolean;
var
  IsHttps: Boolean;
  hConnect, hRequest: HINTERNET;
  SizeStr: array[0..63] of Char;
  BufLen, Index: DWORD;
  i: Integer;
  Port: Word;
  Flag: Cardinal;
begin
  Result := False;
  hConnect := nil;
  hRequest := nil;
  try
    IsHttps := SameText(Info.Protocol, 'https');
    if IsHttps then
    begin
      Port := StrToIntDef(Info.Port, INTERNET_DEFAULT_HTTPS_PORT);
      Flag := INTERNET_FLAG_RELOAD or INTERNET_FLAG_SECURE or
        INTERNET_FLAG_IGNORE_CERT_CN_INVALID or INTERNET_FLAG_IGNORE_CERT_DATE_INVALID;
    end
    else
    begin
      Port := StrToIntDef(Info.Port, INTERNET_DEFAULT_HTTP_PORT);
      Flag := INTERNET_FLAG_RELOAD;
    end;

    hConnect := InternetConnect(hSession, PChar(Info.Host), Port, nil, nil,
      INTERNET_SERVICE_HTTP, 0, 0);
    if (hConnect = nil) or FAborted then
      Exit;

    hRequest := HttpOpenRequest(hConnect, 'GET', PChar(EncodeURL(Info.PathName)),
      HTTP_VERSION, nil, nil, Flag, 0);
    if (hRequest = nil) or FAborted then
      Exit;

    for i := 0 to FHttpRequestHeaders.Count - 1 do
      HttpAddRequestHeaders(hRequest, PChar(FHttpRequestHeaders[i]),
        Length(FHttpRequestHeaders[i]), HTTP_ADDREQ_FLAG_REPLACE or HTTP_ADDREQ_FLAG_ADD);

    if HttpSendRequest(hRequest, nil, 0, nil, 0) then
    begin
      if FAborted then Exit;

      FillChar(SizeStr, SizeOf(SizeStr), 0);
      BufLen := SizeOf(SizeStr);
      Index := 0;
      HttpQueryInfo(hRequest, HTTP_QUERY_CONTENT_LENGTH, @SizeStr, BufLen, Index);
        
      if FAborted then Exit;

      Result := GetStreamFromHandle(hRequest, StrToIntDef(SizeStr, -1), Stream);
    end;
  finally
    if hRequest <> nil then InternetCloseHandle(hRequest);
    if hConnect <> nil then InternetCloseHandle(hConnect);
  end;
end;

function TCnInet.GetString(const AURL: string): AnsiString;
var
  Stream: TMemoryStream;
begin
  try
    Stream := TMemoryStream.Create;
    try
      if GetStream(AURL, Stream) then
      begin
        SetLength(Result, Stream.Size);
        Move(Stream.Memory^, PAnsiChar(Result)^, Stream.Size);
      end
      else
        Result := '';
    finally
      Stream.Free;
    end;
  except
    Result := '';
  end;
end;

function TCnInet.GetFile(const AURL, FileName: string): Boolean;
var
  Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
    try
      Stream.Size := 0;
      Result := GetStream(AURL, Stream);
    finally
      Stream.Free;
    end;
  except
    Result := False;
  end;
end;

end.
