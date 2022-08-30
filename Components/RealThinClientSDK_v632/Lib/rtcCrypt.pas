{
  @html(<b>)
  Encryption Class
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  This unit implements the encryption class @Link(TRtcCrypt),
  which is used by @Link(TRtcClientModule) and @Link(TRtcServerModule)
  to crypt and decrypt all RTC Data when AutoEnctyption is activated.
}

unit rtcCrypt;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,

  rtcTypes,
  rtcFastStrings,

  rtcInfo,
  rtcLog;

type
  TRtcCrypt = class(TRtcObject)
    private
      FCryptKey:RtcByteArray;
      FCryptCode:RtcByteArray;

      CErr:integer;
      CPos:integer;
      CLen:integer;
      CCode:byte;
      CValue:longint;
      CInit:boolean;

      procedure SetCryptCodeEx(const Value: RtcByteArray);

      procedure SetCryptCode(const Value: RtcString);
      function GetCryptCode:RtcString;

    public
      constructor Create;
      destructor Destroy; override;

      procedure Kill; override;

      procedure Init;

      procedure Crypt(var s:RtcString);
      procedure DeCrypt(var s:RtcString);

      procedure CryptEx(var s:RtcByteArray);
      procedure DeCryptEx(var s:RtcByteArray);

      property Key:RtcString read GetCryptCode write SetCryptCode;
      property KeyEx:RtcByteArray read FCryptCode write SetCryptCodeEx;
    end;

procedure Crypt(var s:RtcString; const key:RtcString);
procedure DeCrypt(var s:RtcString; const key:RtcString);

procedure CryptEx(var s:RtcByteArray; const key:RtcByteArray);
procedure DeCryptEx(var s:RtcByteArray; const key:RtcByteArray);

implementation

procedure Crypt(var s:RtcString; const key:RtcString);
  var
    crypt:TRtcCrypt;
  begin
  crypt:=TRtcCrypt.Create;
  try
    crypt.Key:=key;
    crypt.Crypt(s);
  finally
    RtcFreeAndNil(crypt);
    end;
  end;

procedure DeCrypt(var s:RtcString; const key:RtcString);
  var
    crypt:TRtcCrypt;
  begin
  crypt:=TRtcCrypt.Create;
  try
    crypt.Key:=key;
    crypt.DeCrypt(s);
  finally
    RtcFreeAndNil(crypt);
    end;
  end;

procedure CryptEx(var s:RtcByteArray; const key:RtcByteArray);
  var
    crypt:TRtcCrypt;
  begin
  crypt:=TRtcCrypt.Create;
  try
    crypt.KeyEx:=key;
    crypt.CryptEx(s);
  finally
    RtcFreeAndNil(crypt);
    end;
  end;

procedure DeCryptEx(var s:RtcByteArray; const key:RtcByteArray);
  var
    crypt:TRtcCrypt;
  begin
  crypt:=TRtcCrypt.Create;
  try
    crypt.KeyEx:=key;
    crypt.DeCryptEx(s);
  finally
    RtcFreeAndNil(crypt);
    end;
  end;

{ TRtcCrypt }

constructor TRtcCrypt.Create;
  begin
  inherited;
  SetLength(FCryptKey,0);
  SetLength(FCryptCode,0);
  CInit:=False;
  Init;
  end;

destructor TRtcCrypt.Destroy;
  begin
  try
    SetLength(FCryptKey,0);
    SetLength(FCryptCode,0);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcCrypt.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcCrypt.Init;
  var
    a:integer;
  begin
  if CInit then Exit;

  FCryptCode := Copy(FCryptKey,0,length(FCryptKey)); // Initial encryption key
  CValue:=0;
  CLen:=length(FCryptCode);

  if CLen>0 then
    begin
    // First code = sum of all crypt bytes
    for a:=0 to CLen-1 do
      Inc(CValue,FCryptCode[a]);
      
    if CValue>$FFFF then
      CValue:=(CValue and $FFFF)+(CValue shr 16);
    CErr:=CValue+CLen;

    CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
    if CCode=0 then
      begin
      Inc(CValue,CErr);
      CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
      end;
    CPos:=0;

    CInit:=True;
    end;
  end;

procedure TRtcCrypt.CryptEx(var s: RtcByteArray);
  var
    a:integer;
    c,c2:byte;
  begin
  CInit:=False;
  if CLen>0 then
    begin
    for a:=0 to length(s)-1 do
      begin
      c2:=s[a];
      c:=c2 xor CCode; // Crypt this character

      CValue:=CValue * (1+(c2 and $F)) + (c2 and $F0); // include original character into the code

      if CPos>=CLen then
        CPos:=1
      else
        Inc(CPos);

      Inc(CValue, FCryptCode[CPos-1]);
      if CValue>$FFFF then
        CValue:=(CValue and $FFFF)+(CValue shr 16);

      CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
      if CCode=0 then
        begin
        Inc(CValue,CErr);
        CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
        end;
      s[a]:=c;
      end;
    end;
  end;

procedure TRtcCrypt.Crypt(var s: RtcString);
  var
    a:integer;
    c,c2:byte;
  begin
  CInit:=False;
  if CLen>0 then
    begin
  {$IFNDEF RTC_BYTESTRING}
    if RTC_STRING_FIXMODE>=rtcStr_FixDown then
      begin
      for a:=1 to length(s) do
        begin
        if Ord(s[a])<=255 then
          c2:=Byte(s[a])
        else
          begin
          c2:=RtcUnicodeToAnsiChar(Word(s[a]));
          if RTC_STRING_CHECK and (c2=RTC_INVALID_CHAR) then
            raise ERtcInfo.Create('TRtcCrypt.Crypt: Source contains Unicode character #'+IntToStr(Ord(s[a]))+' = '+s[a]);
          end;
        c:=c2 xor CCode; // Crypt this character

        CValue:=CValue * (1+(c2 and $F)) + (c2 and $F0); // include original character into the code

        if CPos>=CLen then
          CPos:=1
        else
          Inc(CPos);

        Inc(CValue, FCryptCode[CPos-1]);
        if CValue>$FFFF then
          CValue:=(CValue and $FFFF)+(CValue shr 16);

        CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
        if CCode=0 then
          begin
          Inc(CValue,CErr);
          CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
          end;
        s[a]:=RtcChar(c);
        end;
      end
    else
  {$ENDIF}
      begin
      for a:=1 to length(s) do
        begin
        c2:=Byte(s[a]);
        c:=c2 xor CCode; // Crypt this character

        CValue:=CValue * (1+(c2 and $F)) + (c2 and $F0); // include original character into the code

        if CPos>=CLen then
          CPos:=1
        else
          Inc(CPos);

        Inc(CValue, FCryptCode[CPos-1]);
        if CValue>$FFFF then
          CValue:=(CValue and $FFFF)+(CValue shr 16);

        CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
        if CCode=0 then
          begin
          Inc(CValue,CErr);
          CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
          end;
        s[a]:=RtcChar(c);
        end;
      end;
    end;
  end;

procedure TRtcCrypt.DeCryptEx(var s: RtcByteArray);
  var
    a:integer;
    c:byte;
  begin
  CInit:=False;
  if CLen>0 then
    begin
    for a:=0 to length(s)-1 do
      begin
      c:=s[a] xor CCode; // Crypt this character

      CValue:=CValue * (1+(c and $F)) + (c and $F0); // include original character into the code

      if CPos>=CLen then
        CPos:=1
      else
        Inc(CPos);

      Inc(CValue, FCryptCode[CPos-1]);
      if CValue>$FFFF then
        CValue:=(CValue and $FFFF)+(CValue shr 16);

      CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
      if CCode=0 then
        begin
        Inc(CValue,CErr);
        CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
        end;
      s[a]:=c;
      end;
    end;
  end;

procedure TRtcCrypt.DeCrypt(var s: RtcString);
  var
    a:integer;
    c:byte;
  begin
  CInit:=False;
  if CLen>0 then
    begin
  {$IFNDEF RTC_BYTESTRING}
    if RTC_STRING_FIXMODE>=rtcStr_FixDown then
      begin
      for a:=1 to length(s) do
        begin
        if Ord(s[a])<=255 then
          c:=Byte(s[a])
        else
          begin
          c:=RtcUnicodeToAnsiChar(Word(s[a]));
          if RTC_STRING_CHECK and (c=RTC_INVALID_CHAR) then
            raise ERtcInfo.Create('TRtcCrypt.DeCrypt: Source contains Unicode character #'+IntToStr(Ord(s[a]))+' = '+s[a]);
          end;
        
        c:=c xor CCode; // Crypt this character

        CValue:=CValue * (1+(c and $F)) + (c and $F0); // include original character into the code

        if CPos>=CLen then
          CPos:=1
        else
          Inc(CPos);

        Inc(CValue, FCryptCode[CPos-1]);
        if CValue>$FFFF then
          CValue:=(CValue and $FFFF)+(CValue shr 16);

        CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
        if CCode=0 then
          begin
          Inc(CValue,CErr);
          CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
          end;

        if RTC_STRING_FIXMODE>=rtcStr_FixUpDown then
          s[a]:=RtcChar( RtcAnsiToUnicodeChar(c) )
        else
          s[a]:=RtcChar(c);
        end;
      end
    else
  {$ENDIF}
      begin
      for a:=1 to length(s) do
        begin
        c:=byte(s[a]) xor CCode; // Crypt this character

        CValue:=CValue * (1+(c and $F)) + (c and $F0); // include original character into the code

        if CPos>=CLen then
          CPos:=1
        else
          Inc(CPos);

        Inc(CValue, FCryptCode[CPos-1]);
        if CValue>$FFFF then
          CValue:=(CValue and $FFFF)+(CValue shr 16);

        CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
        if CCode=0 then
          begin
          Inc(CValue,CErr);
          CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
          end;
        s[a]:=RtcChar(c);
        end;
      end;
    end;
  end;

procedure TRtcCrypt.SetCryptCodeEx(const Value: RtcByteArray);
  begin
  FCryptKey := Copy(Value,0,length(Value));
  CInit:=False;
  Init;
  end;

procedure TRtcCrypt.SetCryptCode(const Value: RtcString);
  begin
  FCryptKey := RtcStringToBytes(Value);
  CInit:=False;
  Init;
  end;

function TRtcCrypt.GetCryptCode: RtcString;
  begin
  Result:= RtcBytesToString(FCryptKey);
  end;

procedure TRtcCrypt.Kill;
  begin
  {$IFNDEF NEXTGEN} Free; {$ENDIF}
  end;

end.
