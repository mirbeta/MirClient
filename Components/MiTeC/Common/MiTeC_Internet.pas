{*******************************************************}
{               MiTeC Internet Routines                 }
{                                                       }
{          Copyright (c) 2009-2017 Michal Mutl          }
{                                                       }
{*******************************************************}


{$INCLUDE Compilers.inc}

unit MiTeC_Internet;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, WinAPI.WinInet;
     {$ELSE}
     Windows, SysUtils, Classes, WinInet;
     {$ENDIF}

function DownloadFile(AURL,AUsername,APassword: string; APort: Word; AContent: TStream): Boolean;
function DownloadOutput(AURL,AUsername,APassword: string; APort: Word; AContent: TStream): Boolean;

implementation

function DownloadFile(AURL,AUsername,APassword: string; APort: Word; AContent: TStream): Boolean;
var
  hSession,hConnect,hRequest: hInternet;
  HostName,FileName: string;
  Buffer: Pointer;
  fs,n,c,InternetFlag,BufferLength,i: Cardinal;
  Data: TByteArray;
  https: Boolean;
  pp: Integer;
  p: Word;
const
  {$IFDEF UNICODE}
  AcceptType: packed array[0..1] of LPWSTR = (PChar('*/*'), nil);
  {$else}
  AcceptType: packed array[0..1] of LPSTR = ({$IFNDEF FPC}PChar{$ENDIF}('*/*'), nil);
  {$ENDIF}
begin
  fs:=0;
  p:=APort;
  if APort=0 then
    p:=80;
  AContent.Size:=0;
  AContent.Position:=0;
  n:=0;
  https:=False;
  if Pos('http://',LowerCase(AURL))<>0 then
    System.Delete(AURL,1,7);
  if Pos('https://',LowerCase(AURL))<>0 then begin
    System.Delete(AURL,1,8);
    https:=True;
    if APort=0 then
      p:=443;
  end;
  pp:=Pos('/',AURL);
  HostName:=Copy(AURL,1,pp);
  FileName:=Copy(AURL,pp,Length(AURL)-pp+1);
  if (Length(HostName)>0) and (HostName[Length(HostName)]='/' ) then
    SetLength(HostName,Length(HostName)-1);
  hSession:=InternetOpen('agent',INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0);
  if Assigned(hSession) then
    try
      hConnect:=InternetConnect(hSession,PChar(HostName),p,PChar(AUserName),PChar(APassword),INTERNET_SERVICE_HTTP,0,0);
      if Assigned(hConnect) then
        try
          InternetFlag:=INTERNET_FLAG_RELOAD;
          if https then
            InternetFlag:=InternetFlag
                          or INTERNET_FLAG_SECURE
                          or INTERNET_FLAG_IGNORE_CERT_CN_INVALID
                          or INTERNET_FLAG_IGNORE_CERT_DATE_INVALID
                          or SECURITY_FLAG_IGNORE_UNKNOWN_CA;
          hRequest:=HttpOpenRequest(hConnect,'GET',PChar(FileName),'HTTP/1.0',nil,@AcceptType,InternetFlag,0);
          if Assigned(hRequest) then
            try
              if HttpSendRequest(hRequest,nil,0,nil,0) then begin
                i:=0;
                BufferLength:=SizeOf(TByteArray);
                GetMem(Buffer,BufferLength);
                try
                  if HttpQueryInfo(hRequest,HTTP_QUERY_CONTENT_LENGTH,Buffer,BufferLength,i) then begin
                    fs:=StrToIntDef(string(Buffer),0);
                    n:=0;
                    while True do begin
                      if not InternetReadFile(hRequest,@Data,SizeOf(Data),c) then
                        Break
                      else begin
                        if c=0 then
                          Break
                        else
                          AContent.Write(Data,c);
                        Inc(n,c);
                      end;
                    end;
                  end;
                finally
                  FreeMem(Buffer);
                end;
              end;
            finally
              InternetCloseHandle(hRequest);
            end;
          finally
            InternetCloseHandle(hConnect);
          end;
      finally
        InternetCloseHandle(hSession);
      end;
  Result:=(n>0) and (n=fs);
  AContent.Position:=0;
end;

function DownloadOutput(AURL,AUsername,APassword: string; APort: Word; AContent: TStream): Boolean;
var
  hSession,hConnect,hRequest: hInternet;
  HostName,FileName: string;
  Buffer: Pointer;
  n,c,InternetFlag,BufferLength,i: Cardinal;
  Data: TByteArray;
  https: Boolean;
  p: Word;
  pp: Integer;
const
  {$IFDEF UNICODE}
  AcceptType: packed array[0..1] of LPWSTR = (PChar('*/*'), nil);
  {$else}
  AcceptType: packed array[0..1] of LPSTR = ({$IFNDEF FPC}PChar{$ENDIF}('*/*'), nil);
  {$ENDIF}
begin
  p:=APort;
  if APort=0 then
    p:=80;
  AContent.Size:=0;
  AContent.Position:=0;
  n:=0;
  https:=False;
  if Pos('http://',LowerCase(AURL))<>0 then
    System.Delete(AURL,1,7);
  if Pos('https://',LowerCase(AURL))<>0 then begin
    System.Delete(AURL,1,8);
    https:=True;
    if APort=0 then
      p:=443;
  end;
  pp:=Pos('/',AURL);
  HostName:=Copy(AURL,1,pp);
  FileName:=Copy(AURL,pp,Length(AURL)-pp+1);
  if (Length(HostName)>0) and (HostName[Length(HostName)]='/' ) then
    SetLength(HostName,Length(HostName)-1);
  hSession:=InternetOpen('agent',INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0);
  if Assigned(hSession) then
    try
      hConnect:=InternetConnect(hSession,PChar(HostName),p,PChar(AUserName),PChar(APassword),INTERNET_SERVICE_HTTP,0,0);
      if Assigned(hConnect) then
        try
          InternetFlag:=INTERNET_FLAG_RELOAD;
          if https then
            InternetFlag:=InternetFlag
                          or INTERNET_FLAG_SECURE
                          or INTERNET_FLAG_IGNORE_CERT_CN_INVALID
                          or INTERNET_FLAG_IGNORE_CERT_DATE_INVALID
                          or SECURITY_FLAG_IGNORE_UNKNOWN_CA;
          hRequest:=HttpOpenRequest(hConnect,'GET',PChar(FileName),'HTTP/1.0',nil,@AcceptType,InternetFlag,0);
          if Assigned(hRequest) then
            try
              if HttpSendRequest(hRequest,nil,0,nil,0) then begin
                i:=0;
                BufferLength:=SizeOf(TByteArray);
                GetMem(Buffer,BufferLength);
                try
                  if HttpQueryInfo(hRequest,HTTP_QUERY_RAW_HEADERS,Buffer,BufferLength,i) then begin
                    n:=0;
                    while True do begin
                      if not InternetReadFile(hRequest,@Data,SizeOf(Data),c) then
                        Break
                      else begin
                        if c=0 then
                          Break
                        else
                          AContent.Write(Data,c);
                        Inc(n,c);
                      end;
                    end;
                  end;
                finally
                  FreeMem(Buffer);
                end;
              end;
            finally
              InternetCloseHandle(hRequest);
            end;
          finally
            InternetCloseHandle(hConnect);
          end;
      finally
        InternetCloseHandle(hSession);
      end;
  Result:=(n>0);
  AContent.Position:=0;
end;

end.
