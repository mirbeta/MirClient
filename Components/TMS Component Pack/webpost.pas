{*******************************************************************}
{ TWEBPOST component                                                }
{ for Delphi & C++Builder                                           }
{                                                                   }
{ written by                                                        }
{    TMS Software                                                   }
{     copyright © 2001 - 2015                                       }
{     Email : info@tmssoftware.com                                  }
{     Web   : http://www.tmssoftware.com                            }
{                                                                   }
{ The source code is given as is. The author is not responsible     }
{ for any possible damage done due to the use of this code.         }
{ The component can be freely used in any application. The source   }
{ code remains property of the writer and may not be distributed    }
{ freely as such.                                                   }
{*******************************************************************}

unit WebPost;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, WinInet
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.1.0 : improved handling of post through https
  // v1.0.2.0 : Added property WaitResponse
  // v1.0.3.0 : Improved timeout & error handling
  // v1.0.3.1 : Fixed : issue with Delphi 2009
  // v1.0.3.2 : Fixed : issue with Agent in Delphi 2009
  // v1.0.4.0 : Improved : support to post via HTTPS
  // v1.1.0.0 : New : Referer property added
  // v1.2.0.0 : New : OnProgress event added
  // v1.2.0.1 : Improved : Special character encoding

type

  TWebPostError = procedure(Sender:TObject;ErrorStr:string;ErrorCode:integer) of object;

  TWebPostProgressEvent = procedure(Sender: TObject; Position, Total: DWORD) of object;

  TWebPostItem = class(TCollectionItem)
  private
    FValue: ansistring;
    FName: ansistring;
  public
    constructor Create(Collection: TCollection); override;
  published
    property Name: ansistring read fName write fName;
    property Value: ansistring read fValue write fValue;
  end;

  TWebPostItems = class(TCollection)
  private
   fOwner:TComponent;
   function GetItem(Index: Integer): TWebPostItem;
   procedure SetItem(Index: Integer; Value: TWebPostItem);
  protected
   function GetOwner: tPersistent; override;
  public
   constructor Create(aOwner:TComponent);
   function Add:TWebPostItem;
   function Insert(index:integer): TWebPostItem;
   property Items[Index: Integer]: TWebPostItem read GetItem write SetItem;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebPost = class(TComponent)
  private
    { Private declarations }
    FServer: string;
    FAction: string;
    FItems: TWebPostItems;
    FPostResult: string;
    FOnError: TWebPostError;
    FPort: Integer;
    FAgent: Ansistring;
    FTimeOut: integer;
    FWaitResponse: boolean;
    FReferer: string;
    FOnProgress: TWebPostProgressEvent;
    procedure SetItems(const Value: TWebPostItems);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
  protected
    { Protected declarations }
    procedure DoProgress(Position, Total: DWORD); virtual;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    function Execute:boolean;
    procedure Error;
    procedure SaveToFile(fn:string);
    property PostResult:string read fPostResult;
  published
    { Published declarations }
    property Agent: Ansistring read FAgent write FAgent;
    property Port: Integer read FPort write FPort;
    property Server: string read FServer write FServer;
    property Action: string read FAction write FAction;
    property Items: TWebPostItems read fItems write SetItems;
    property Referer: string read FReferer write FReferer;
    property TimeOut: integer read FTimeOut write FTimeOut default 0;
    property Version: string read GetVersion write SetVersion;
    property WaitResponse: boolean read FWaitResponse write FWaitResponse default true;    
    property OnError: TWebPostError read FOnError write FOnError;
    property OnProgress: TWebPostProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

const
 READBUFFERSIZE = 4096;
 winetdll = 'WININET.DLL';


{ TWebPostItem }

constructor TWebPostItem.Create(Collection: TCollection);
begin
  inherited;
end;

{ TWebPostItems }

function TWebPostItems.Add: TWebPostItem;
begin
  Result := TWebPostItem(inherited Add);
end;

constructor TWebPostItems.Create(aOwner:TComponent);
begin
  inherited Create(TWebPostItem);
  FOwner := AOwner;
end;


function TWebPostItems.GetItem(Index: Integer): TWebPostItem;
begin
  Result := TWebPostItem(inherited GetItem(Index));
end;

function TWebPostItems.GetOwner: tPersistent;
begin
  Result := FOwner;
end;

function TWebPostItems.Insert(index: integer): TWebPostItem;
begin
  Result := TWebPostItem(inherited Insert(index));
end;

procedure TWebPostItems.SetItem(Index: Integer; Value: TWebPostItem);
begin
  inherited SetItem(Index, Value);
end;


{ TWebPost }

constructor TWebPost.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TWebPostItems.Create(Self);
  FAgent := 'WebPost';
  FWaitResponse := true;
end;

destructor TWebPost.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TWebPost.DoProgress(Position, Total: DWORD);
begin
  if Assigned(OnProgress) then
    OnProgress(Self, Position, Total);
end;

procedure TWebPost.Error;
var
  Errorcode:dword;
  dwIntError,dwLength:dword;
  buf:array[0..1024] of char;

begin
  ErrorCode := GetLastError;
  if (ErrorCode <> 0) then
  begin
    FormatMessage(FORMAT_MESSAGE_FROM_HMODULE,
     pointer(GetModuleHandle(winetdll)),ErrorCode,0,buf,sizeof(buf),nil);

    if (ErrorCode = ERROR_INTERNET_EXTENDED_ERROR) then
    begin
      InternetGetLastResponseInfo(dwIntError,nil,dwLength);
      if (dwLength>0) then
      begin
        InternetGetLastResponseInfo(dwIntError,buf,dwLength);

        if Assigned(fOnError) then
          FOnError(self,strpas(buf),ErrorCode)
        else
          Messagedlg(strpas(buf),mtError,[mbok],0);
      end
    end
    else
    begin
      if Assigned(fOnError) then
        FOnError(self,strpas(buf),ErrorCode)
      else
        Messagedlg(strpas(buf),mtError,[mbok],0);
     end;
   end;
end;


function URLEncode(const Url: string): string;
var
  i: Integer;
  UrlA: ansistring;
  res: ansistring;

begin
  res := '';
  UrlA := ansistring(UTF8Encode(Url));

  for i := 1 to Length(UrlA) do
  begin
    case UrlA[i] of
      'A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.':
        res := res + UrlA[i];
    else
        res := res + '%' + ansistring(IntToHex(Ord(UrlA[i]), 2));
    end;
  end;

  Result := string(res);
end;

function TWebPost.Execute:boolean;
var
  hint,hconn,hreq:hinternet;
  hdr:ansistring;
  buf:array[0..READBUFFERSIZE-1] of ansichar;
  bufsize,position: DWORD;
  i,flags:integer;
  data: ansistring;
  value: ansistring;
  dwSize, dwFlags: DWORD;

begin
  Result := False;

  hdr := 'Content-Type: application/x-www-form-urlencoded';

  data := '';

  for i := 1 to FItems.Count do
  begin
    if data <> '' then
      data := data + '&';

    value := fItems.Items[i - 1].Value;
    while (pos(' ',string(value)) > 0) do
      value[pos(' ',string(value))] := '+';

    data := data + FItems.Items[i - 1].Name + '=' + ansistring(URLEncode(string(value)));
  end;

  hint := InternetOpenA(PAnsiChar(FAgent),INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0);

  if hint = nil then
    Error
  else
    try
      if FTimeout > 0 then
      begin
        InternetSetOption(hint, INTERNET_OPTION_CONNECT_TIMEOUT, @FTimeout, SizeOf(FTimeout));
        InternetSetOption(hint, INTERNET_OPTION_SEND_TIMEOUT, @FTimeout, SizeOf(FTimeout));
        InternetSetOption(hint, INTERNET_OPTION_RECEIVE_TIMEOUT, @FTimeout, SizeOf(FTimeout));
      end;

      if FPort = 0 then
        hconn := InternetConnect(hint,PChar(FServer),INTERNET_DEFAULT_HTTP_PORT,nil,nil,INTERNET_SERVICE_HTTP,0,1)
      else
        hconn := InternetConnect(hint,PChar(FServer),FPort,nil,nil,INTERNET_SERVICE_HTTP,0,1);

      if hconn = nil then
        Error
      else
      try
        if FPort = INTERNET_DEFAULT_HTTPS_PORT then
          flags := INTERNET_FLAG_SECURE or INTERNET_FLAG_IGNORE_CERT_CN_INVALID or INTERNET_FLAG_IGNORE_CERT_DATE_INVALID
        else
          flags := 0;

        hreq := HttpOpenRequest(hconn, 'POST', pchar(FAction), nil, pchar(FReferer), nil, flags, 1);

        if Assigned(hreq) and (FPort = INTERNET_DEFAULT_HTTPS_PORT) then
        begin
          dwSize := SizeOf(dwFlags);
          if (InternetQueryOption(hreq, INTERNET_OPTION_SECURITY_FLAGS, @dwFlags, dwSize)) then
          begin
            dwFlags := dwFlags or SECURITY_FLAG_IGNORE_UNKNOWN_CA;
            if not (InternetSetOption(hreq, INTERNET_OPTION_SECURITY_FLAGS, @dwFlags, dwSize)) then
            begin
              Error;
            end;
          end
          else
          begin
            Error;  //InternetQueryOption failed
          end;
        end;

        if hreq = nil then
          Error
        else
        try
          FPostResult := '';

          if HttpSendRequestA(hreq,pansichar(hdr),length(hdr),pansichar(Data),length(Data)) then
          begin
            if FWaitResponse then
            begin
              bufsize := READBUFFERSIZE;
              Result := True;
              position := 0;

              while (bufsize > 0) do
              begin
                Application.ProcessMessages;
                if not InternetReadFile(hreq,@buf,READBUFFERSIZE,bufsize) then
                begin
                  Result := False;
                  break;
                end;

                position := position + bufsize;

                DoProgress(position,0);

                if (bufsize > 0) and (bufsize <= READBUFFERSIZE) then
                  for i := 0 to bufsize - 1 do
                    FPostResult := FPostResult + string(buf[i]);
              end;
            end;
          end
          else
            Error;
      finally
        InternetCloseHandle(hreq);
      end;
    finally
      InternetCloseHandle(hconn);
    end;
  finally
    InternetCloseHandle(hint);
  end;
end;

procedure TWebPost.SaveToFile(fn: string);
var
  tf:text;

begin
  Assignfile(tf,fn);
  {$i-}
  Rewrite(tf);
  {$i+}
  if IOResult = 0 then
  begin
    write(tf,fPostResult);
    closefile(tf);
  end
  else
    raise Exception.Create('Cannot create file');
end;

procedure TWebPost.SetItems(const Value: TWebPostItems);
begin
  FItems.Assign(Value);
end;

function TWebPost.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TWebPost.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TWebPost.SetVersion(const Value: string);
begin

end;

end.
