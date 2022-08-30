{*******************************************************}
{                MiTeC Common Routines                  }
{               Simple EML file parser                  }
{                                                       }
{          Copyright (c) 2009-2017 Michal Mutl          }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_EML;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes;
     {$ELSE}
     Windows, SysUtils, Classes;
     {$ENDIF}

type
  TEMLHeader = record
    Date: string;
    Timestamp: TDateTime;
    UserName: string;
    From: string;
    Sender: string;
    ReplyTo: string;
    ReturnPath: string;
    SendTo: string;
    CC: string;
    BCC: string;
    Subject: string;
    Priority: cardinal;
    ContentTransferEncoding: string;
    MessageId: string;
    ContentType: string;
    Size: Int64;

    RevisionID,
    DocumentID: string;

    Attachments,
    Read: Boolean;
  end;

function WideToAnsi(const ws: WideString; codePage: Word = CP_ACP): AnsiString;
function AnsiToWide(const s: AnsiString; codePage: Word = CP_ACP): WideString;
procedure ParseEML(AMessage: TStrings; var AHeader: TEMLHeader; AOnlyMozilla: Boolean = False);
function GetEmailAddressParts(const ACompleteEmail: string; var AName, AEmail: string): Boolean;
function MailDateToDateTime(const ADateStr: string): TDateTime;

implementation

{$IFDEF RAD12PLUS}uses System.AnsiStrings;{$ENDIF}

function WideToAnsi(const ws: WideString; codePage: Word = CP_ACP): AnsiString;
var
  l: integer;
  f: Cardinal;
begin
  f:=WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR;
  if codepage=CP_UTF8 then
    f:=0;
  if ws = '' then
    Result:=''
  else begin
    l:=WideCharToMultiByte(codePage,f,@ws[1],-1,nil,0,nil,nil);
    SetLength(Result,l-1);
    if l>1 then
      WideCharToMultiByte(codePage,f,@ws[1],-1,@Result[1],l-1,nil,nil);
  end;
end;

function AnsiToWide(const s: AnsiString; codePage: Word = CP_ACP): WideString;
var
  l: integer;
  f: Cardinal;
begin
  f:=MB_PRECOMPOSED;
  if codepage=CP_UTF8 then
    f:=0;
  if s = '' then
    Result:=''
  else begin
    l:=MultiByteToWideChar(codePage,f,PAnsiChar(@s[1]),-1,nil,0);
    SetLength(Result,l-1);
    if l>1 then
      MultiByteToWideChar(CodePage,f,PAnsiChar(@s[1]),-1,PWideChar(@Result[1]),l-1);
  end;
end;

function IsBitOn (Value: Integer; Bit: Byte): Boolean;
begin
  Result:=(Value and (1 shl Bit))<>0;
end;

function TrimSpace(const S: string): string;
var
  I, L: Integer;
begin
  L:=Length(S);
  I:=1;
  while (I <= L) and {$IFDEF UNICODE}CharInSet(S[I],[#9, #32]){$else}(S[I] in [#9, #32]){$ENDIF} do
    Inc(I);
  if I > L then
    Result:=''
  else begin
    while S[L] = ' ' do
      Dec(L);
    Result:=Copy(S, I, L - I + 1);
  end;
end;

function FastStringReplace(const ASource, APattern, AReplace: string): string;
var
  i: Integer;
  s: string;
begin
  s:=ASource;
  Result:='';
  repeat
    i:=Pos(APattern,s);
    if i>0 then begin
      Result:=Result+Copy(s,1,i- 1)+AReplace;
      s:=Copy(s,i+Length(APattern),MaxInt);
    end else
      Result:=Result+s;
  until i<=0;
end;

function GetTimeZoneBias: Double;
var
  TzInfo: TTimeZoneInformation;

begin
  case GetTimeZoneInformation(TzInfo) of
    1: Result:=-(TzInfo.StandardBias+TzInfo.Bias)/(24*60);
    2: Result:=-(TzInfo.DaylightBias+TzInfo.Bias)/(24*60);
    else Result:=-TzInfo.Bias/(24*60);
  end;
end;

function MailDateToDateTime(const ADateStr: string): TDateTime;
const
  Months: String = 'Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,';
var
  Field,i: Integer;
  Hour, Min, Sec, Year, Month, Day: Double;
  sDate, sHour, sMin, sSec, sYear, sMonth, sDay, sTZ: String;
  HTZM, MTZM: Word;
  STZM: Integer;
  TZM: Double;
  fdt: Double;
begin
  sHour:='';
  sMin:='';
  sSec:='';
  sYear:='';
  sMonth:='';
  sDay:='';
  sTZ:='';
  sDate:=FastStringReplace(Trim(ADateStr),'  ',' ');
  if sDate<>'' then begin
    if {$IFDEF UNICODE}CharInSet(sDate[1],['0'..'9']){$else}(sDate[1] in ['0'..'9']){$ENDIF} then
      Field:=1
    else
      Field:=0;
    for i:=1 to Length(sDate) do begin
      if {$IFDEF UNICODE}CharInSet(sDate[i],[#32, ':', '/']){$else}(sDate[i] in [#32, ':', '/']){$ENDIF} then begin
        Inc(Field);
        if (Field = 6) and (sDate[i] = #32) then
          Field:=7;
      end else begin
        case Field of
          1: sDay:=sDay + sDate[i];
          2: sMonth:=sMonth + sDate[i];
          3: sYear:=sYear + sDate[i];
          4: sHour:=sHour + sDate[i];
          5: sMin:=sMin + sDate[i];
          6: sSec:=sSec + sDate[i];
          7: sTZ:=sTZ + sDate[i];
        end;
      end;
    end;
    Hour:=StrToIntDef(sHour,0);
    Min:=StrToIntDef(sMin,0);
    Sec:=StrToIntDef(sSec,0);
    Year:=StrToIntDef(sYear,0);
    Day:=StrToIntDef(sDay,0);
    if {$IFDEF UNICODE}CharInSet(sMonth[1],['0'..'9']){$else}(sMonth[1] in ['0'..'9']){$ENDIF} then
      Month:=StrToIntDef(sMonth,0)
    else
      Month:=(Pos(sMonth, Months)-1) div 4 + 1;
    if Year<100 then begin
      if Year<50 then
        Year:=2000+Year
      else
        Year:=1900+Year;
    end;

    if (Year=0) or (Month=0) or (Year=0) then
      Result:=0
    else begin
      if (sTZ='GMT') or (Length(Trim(sTZ))<>5) then begin
        STZM:=1;
        HTZM:=0;
        MTZM:=0;
      end else begin
        STZM:=StrToIntDef(Copy(sTZ,1,1)+'1',1);
        HTZM:=StrToIntDef(Copy(sTZ,2,2),0);
        MTZM:=StrToIntDef(Copy(sTZ,4,2),0);
      end;

      try
        TZM:=EncodeTime(HTZM, MTZM,0,0)*STZM;
        fdt:=EncodeDate(Trunc(Year),Trunc(Month),Trunc(Day));
        fdt:=fdt+Hour*(1/24)+Min*(1/24/60)+Sec*(1/24/60/60);
        fdt:=fdt-TZM+GetTimeZoneBias;
        Result:=fdt;
      except
        Result:=0;
      end;
    end;
  end else
    Result:=0;
end;

function QuotedPrintableDecode(const FCurrentData: PAnsiChar): string;
{ This works if charset="iso-8859-1" ! }
var
  SourceIndex,
  DecodedIndex,
  CharCode: Integer;
  Ch: AnsiChar;
  CodeHex: string;
begin
  SourceIndex:=0;
  DecodedIndex:=0;
  if (FCurrentData<>'' ) and (FCurrentData^<>#0) then begin
    while True do begin
      Ch:=FCurrentData[SourceIndex];
      if Ch=#0 then
         Break;
      if Ch='_' then begin
        FCurrentData[DecodedIndex]:=' ';
        Inc(SourceIndex);
        Inc(DecodedIndex);
      end else
      if Ch<>'=' then begin
        FCurrentData[DecodedIndex]:=Ch;
        Inc(SourceIndex);
        Inc(DecodedIndex);
      end else begin
        Inc(SourceIndex);
        Ch:=FCurrentData[SourceIndex];
        if (Ch=#13) or (Ch=#10) then begin
          Inc(SourceIndex);
          Inc(SourceIndex);
        end else begin
          CodeHex:='$'+Ch;
          Inc(SourceIndex);
          Ch:=FCurrentData[SourceIndex];
          if Ch=#0 then
            Break;
          CodeHex:=CodeHex+string(Ch);
          CharCode:=StrToIntDef(CodeHex,64);
          case CharCode of
            $B9: FCurrentData[DecodedIndex]:='š';
            $BE: FCurrentData[DecodedIndex]:='ž';
            else FCurrentData[DecodedIndex]:=AnsiChar(Chr(CharCode));
          end;
          Inc(SourceIndex);
          Inc(DecodedIndex);
        end;
      end;
    end;
    FCurrentData[DecodedIndex]:=#0;
  end;
  Result:=string(FCurrentData);
end;

function DecodeLineBASE64(const Buffer: AnsiString; Decoded: PAnsiChar): Integer;
var
  A1: array[1..4] of Byte;
  B1: array[1..3] of Byte;
  I, J: Integer;
  BytePtr, RealBytes: Integer;
begin
  BytePtr:=0;
  Result:=0;
  for J:=1 to Length(Buffer) do begin
    Inc(BytePtr);
    case Buffer[J] of
      'A'..'Z': A1[BytePtr]:=Ord(Buffer[J])-65;
      'a'..'z': A1[BytePtr]:=Ord(Buffer[J])-71;
      '0'..'9': A1[BytePtr]:=Ord(Buffer[J])+4;
      '+': A1[BytePtr]:=62;
      '/': A1[BytePtr]:=63;
      '=': A1[BytePtr]:=64;
    end;
    if BytePtr = 4 then begin
      BytePtr:=0;
      RealBytes:=3;
      if A1[1] = 64 then
        RealBytes:=0;
      if A1[3] = 64 then begin
        A1[3]:=0;
        A1[4]:=0;
        RealBytes:=1;
      end;
      if A1[4] = 64 then begin
        A1[4]:=0;
        RealBytes:=2;
      end;
      B1[1]:=A1[1]*4 + (A1[2] div 16);
      B1[2]:=(A1[2] mod 16)*16+(A1[3] div 4);
      B1[3]:=(A1[3] mod 4)*64 + A1[4];
      for I:=1 to RealBytes do begin
        Decoded[Result+I-1]:=AnsiChar(Chr(B1[I]));
      end;
      Inc(Result, RealBytes);
    end;
  end;
end;

function DecodeQuotedPrintable(const Texto: String): String;
var
  nPos: Integer;
  nLastPos: Integer;
//lFound: Boolean;
begin
  Result:='';
  nPos:=1;
  while nPos <= Length(Texto) do begin
    if Texto[nPos] = '=' then begin
      if (Length(Texto)-nPos) >= 2 then begin
        if (Texto[nPos+1] = #13) and (Texto[nPos+2] = #10) then begin
          Inc(nPos, 3);
        end else begin
          if {$IFDEF UNICODE}CharInSet(Texto[nPos+1],['0'..'9', 'A'..'F']){$else}(Texto[nPos+1] in ['0'..'9', 'A'..'F']){$ENDIF}
             and {$IFDEF UNICODE}CharInSet(Texto[nPos+2],['0'..'9', 'A'..'F']){$ELSE}(Texto[nPos+2] in ['0'..'9', 'A'..'F']){$ENDIF} then begin
            Result:=Result + Char(StrToInt('$'+Texto[nPos+1]+Texto[nPos+2]));
            Inc(nPos, 3)
          end else begin
            Inc(nPos, 3);
          end;
        end;
      end else begin
        Break;
      end;
    end else begin
      nLastPos:=nPos;
      nPos:=Pos('=', Copy(Texto, nLastPos+1, High(Integer)));
      if nPos = 0 then
        nPos:=Length(Texto)+1;
      Result:=Result + Copy(Texto, nLastPos, nPos);
      Inc(nPos, nLastPos);
    end;
  end;
end;

procedure DataLinePChar(const Data: PAnsiChar; const TotalLength: Integer; var LinePos, LineLen: Integer; var Line: PAnsiChar; var DataEnd: Boolean);
begin
  if LinePos >= 0 then begin
    Data[LinePos+LineLen]:=#13;
    LinePos:=LinePos+LineLen+2;
    LineLen:=0;
  end else begin
    LinePos:=0;
    LineLen:=0;
  end;

  while (LinePos+LineLen) < TotalLength do begin
    if Data[LinePos+LineLen] = #13 then begin
      if (LinePos+LineLen+1) < TotalLength then begin
        if Data[LinePos+LineLen+1] = #10 then begin
          Data[LinePos+LineLen]:=#0;
          Line:=@Data[LinePos];
          Exit;
        end;
      end;
    end;
    Inc(LineLen);
  end;
  if LinePos < TotalLength then
    Line:=@Data[LinePos]
  else
    DataEnd:=True;
end;

function DecodeLineUUCODE(const Buffer: AnsiString; Decoded: PAnsiChar): Integer;
const
	CHARS_PER_LINE = 45;
	Table: AnsiString = '`!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_';

var
	A24Bits: array[0..8 * CHARS_PER_LINE] of Boolean;
	i, j, k, b: Word;
	LineLen, ActualLen: Byte;

	function p_ByteFromTable(Ch: AnsiChar): Byte;
	var
		ij: Integer;
	begin
		ij:=Pos(Ch, Table);
		if (ij > 64) or (ij = 0) then begin
			if Ch = #32 then
				Result:=0
      else
     		raise Exception.Create('UUCODE message format error');
		end else
			Result:=ij - 1;
	end;

begin
  if Buffer = '' then begin
    Result:=0;
    Exit;
  end;
  try
    LineLen:=p_ByteFromTable(Buffer[1]);
    ActualLen:=4 * LineLen div 3;
    FillChar(A24Bits, 8 * CHARS_PER_LINE + 1, 0);
    Result:=LineLen;
    if ActualLen <> (4 * CHARS_PER_LINE div 3) then
      ActualLen:=Length(Buffer) - 1;
    k:=0;
    for i:=2 to ActualLen + 1 do begin
      b:=p_ByteFromTable(Buffer[i]);
      for j:=5 downto 0 do begin
        A24Bits[k]:=b and (1 shl j) > 0;
        Inc(k);
      end;
    end;

    k:=0;
    for i:=1 to CHARS_PER_LINE do  begin
      b:=0;
      for j:=7 downto 0 do begin
        if A24Bits[k] then b:=b or (1 shl j);
        Inc(k);
      end;
      Decoded[i-1]:=AnsiChar(Char(b));
    end;

  except

    Result:=0;
  end;
end;

function DecodeUUCODE(Encoded: PAnsiChar; Decoded: TMemoryStream): Boolean;
var
  nTL, nPos, nLen: Integer;
  Line: PAnsiChar;
  LineDec: array[0..79] of AnsiChar;
  LineLen: Integer;
  DataEnd: Boolean;
begin
  Decoded.Clear;
  DataEnd:=False;
  nPos:=-1;
  nTL:={$IFDEF RAD12PLUS}System.AnsiStrings.{$ENDIF}StrLen(Encoded);
  DataLinePChar(Encoded, nTL, nPos, nLen, Line, DataEnd);
  while not DataEnd do begin
    if nLen > 0 then begin
      LineLen:=DecodeLineUUCODE(AnsiString(Line), LineDec);
      if LineLen > 0 then
        Decoded.Write(LineDec[0], LineLen);
    end;
    DataLinePChar(Encoded, nTL, nPos, nLen, Line, DataEnd);
  end;
  Result:=True;
end;

function DecodeLine7Bit(Texto: String): String;
var
  Buffer: PAnsiChar;
  Encoding: Char;
  Size: Integer;
  nPos0: Integer;
  nPos1: Integer;
  nPos2: Integer;
  nPos3: Integer;
  utf,iso8859,Found: Boolean;
begin
  utf:=Pos('=?UTF-8?',Uppercase(Texto))>0;
  iso8859:=(Pos('=?ISO-8859-1?',Uppercase(Texto))>0) or (Pos('=?ISO-8859-2?',Uppercase(Texto))>0);
  Result:=TrimSpace(Texto);
  repeat
    nPos0:=Pos('=?', Result);
    Found:=False;
    if nPos0 > 0 then begin
      nPos1:=Pos('?', Copy(Result, nPos0+2, Length(Result)))+nPos0+1;
      nPos2:=Pos('?=', Copy(Result, nPos1+1, Length(Result)))+nPos1;
      nPos3:=Pos('?', Copy(Result, nPos2+1, Length(Result)))+nPos2;
      if nPos3 > nPos2 then begin
        if Length(Result) > nPos3 then begin
          if Result[nPos3+1] = '=' then begin
            nPos2:=nPos3;
          end;
        end;
      end;

      if (nPos1 > nPos0) and (nPos2 > nPos1) then begin
        Texto:=Copy(Result, nPos1+1, nPos2-nPos1-1);
        if (Length(Texto) >= 2) and (Texto[2] = '?') and
           {$IFDEF UNICODE}CharInSet(UpCase(Texto[1]),['B', 'Q', 'U']){$ELSE}(UpCase(Texto[1]) in ['B', 'Q', 'U']){$ENDIF} then begin
          Encoding:=UpCase(Texto[1]);
        end else begin
          Encoding:='Q';
        end;
        Texto:=Copy(Texto, 3, Length(Texto)-2);
        case Encoding of
          'B': begin
            GetMem(Buffer, Length(Texto));
            {$IFDEF UNICODE}
            Size:=DecodeLineBASE64(WideToAnsi(Texto),Buffer);
            {$else}
            Size:=DecodeLineBASE64(Texto, Buffer);
            {$ENDIF}
            if Size>0 then
              Buffer[Size]:=#0;
            Texto:=string(Buffer);
          end;
          'Q': begin
            while Pos('_', Texto) > 0 do
              Texto[Pos('_', Texto)]:=#32;
            if iso8859 then
              {$IFDEF UNICODE}
              Texto:=QuotedPrintableDecode(PAnsiChar(WideToAnsi(Texto)))
              {$ELSE}
              Texto:=QuotedPrintableDecode(PAnsiChar(Texto))
              {$ENDIF}
            else
              Texto:=DecodeQuotedPrintable(Texto);
          end;
          'U': begin
            GetMem(Buffer, Length(Texto));
            {$IFDEF UNICODE}
            Size:=DecodeLineUUCODE(WideToAnsi(Texto),Buffer);
            {$else}
            Size:=DecodeLineUUCODE(Texto,Buffer);
            {$ENDIF}
            if Size>0 then
              Buffer[Size]:=#0;
            Texto:=string(Buffer);
          end;
        end;
        Result:=Copy(Result, 1, nPos0-1)+Texto+Copy(Result,nPos2+2,Length(Result));
        Found:=True;
      end;
    end;
  until not Found;
  if utf then
    Result:=string({$IFDEF UNICODE}UTF8String{$ELSE}UTF8Decode{$ENDIF}(Result));
end;

function GetFieldValueFromLine(Field, Line: string): string;
var
  p,i: integer;
begin
  Result:='';
  p:=Pos(Field,Uppercase(Line));
  if p>0 then begin
    Result:=Trim(Copy(Line,p+Length(Field),Length(Line)));
    i:=Pos(';',Result);
    if i>0 then
      SetLength(Result,i-1);
  end;
end;

function GetEmailAddressParts(const ACompleteEmail: string; var AName, AEmail: string): Boolean;

function TextPos(const SubStr, Str: string; StartPos: Integer): Integer;
var
  PosRes, StrLen: Integer;
  s: string;
begin
  Result:=0;
  StrLen:=Length(Str);
  if (StartPos < 1) or (StartPos > StrLen) then Exit;
  s:=system.Copy(Str, StartPos, StrLen);
  PosRes:=system.Pos(SubStr, s);
  if (PosRes <> 0) then Result:=StartPos + PosRes - 1;
end;


function GetEmailAddressPartsByDelimiter(indStart: Integer; ADelimiterEnd: string): Boolean;
var
  indEnd: Integer;
begin
  AName:=Trim(system.Copy(ACompleteEmail, 1, indStart - 1));
  indEnd:=TextPos(ADelimiterEnd, ACompleteEmail, indStart + 1);
  Result:=(indEnd > 0);
  if Result then
    AEmail:=Trim(system.Copy(ACompleteEmail, indStart + 1, indEnd - indStart -1));
end;

function GetDenormName(const AName: string): string;
var
  i, j: Integer;
  Len: Integer;
  SpecSymExpect: Boolean;
  Sym: Char;
begin
  SpecSymExpect:=False;
  Len:=Length(AName);
  SetLength(Result, Len);
  i:=1;
  j:=1;
  while (i <= Length(AName)) do begin
    Sym:=AName[i];
    case Sym of
      '\': if not SpecSymExpect then  begin
             SpecSymExpect:=True;
             Inc(i);
             Continue;
           end;
      '"': if not SpecSymExpect then
             Sym:=' ';
    end;
    SpecSymExpect:=False;
    Result[j]:=Sym;
    Inc(j);
    Inc(i);
  end;
  SetLength(Result, j - 1);
end;

var
  indStart: Integer;
begin
  AName:=ACompleteEmail;
  AEmail:=ACompleteEmail;
  indStart:=system.Pos('<', ACompleteEmail);
  Result:=(indStart > 0);
  if Result then
    Result:=GetEmailAddressPartsByDelimiter(indStart, '>')
  else begin
    indStart:=system.Pos('(', ACompleteEmail);
    Result:=(indStart > 0);
    if Result then
      Result:=GetEmailAddressPartsByDelimiter(indStart, ')');
  end;
  if Result then begin
    AName:=Trim(GetDenormName(AName));
    if (Length(AName) > 1) and (AName[1] = '''') and (AName[Length(AName)] = '''') then
      AName:=Copy(AName, 2, Length(AName) - 2);
  end;
end;

function  FindFieldInHeaders(const Field: string; const Headers: TStrings; var Line: string): Boolean;
var
  s: string;
  n,i: integer;
begin
  Result:=false;
  if Headers.Count=0 then
    Exit;
  n:=0;
  while (n<Headers.count) and(Headers[n]='') do
    Inc(n);
  while (n<Headers.count) and (Headers[n]<>'') do begin
    s:=UpperCase(Headers[n]);
    if Pos(Field,s)=1 then begin
      Line:=Headers[n];
      i:=n+1;
      while (i<Headers.Count) and (Headers[i]<>'') and
            ((Headers[i][1]=' ') or (Headers[i][1]=#9)) do begin
        Line:=Line+Trim(Headers[i]);
        inc(i);
      end;
      Result:=true;
      Break;
    end;
    inc(n);
  end;
end;

procedure ParseEML;
var
  i: integer;
  s: string;
  w: Word;
  d: Cardinal;
begin
  Finalize(AHeader);
  AHeader.Priority:=0;
  AHeader.Attachments:=False;
  AHeader.Read:=False;

  if (AMessage.Count=0) then
    Exit;
  i:=0;
  while (i<AMessage.Count) do begin
    if (AMessage[i]='') then
      AMessage.Delete(i)
    else
      inc(i);
  end;

  if AMessage.Count=0 then
    Exit;

  if FindFieldInHeaders('X-MOZILLA-STATUS:',AMessage,s) then begin
    w:=StrToIntDef('$'+Trim(GetFieldValueFromLine('X-MOZILLA-STATUS:',s)),0);
    AHeader.Read:=w and 1=1;
  end;

  if FindFieldInHeaders('X-MOZILLA-STATUS2:',AMessage,s) then begin
    d:=StrToIntDef('$'+Trim(GetFieldValueFromLine('X-MOZILLA-STATUS2:',s)),0);
    AHeader.Attachments:=d and $10000000=$10000000;
  end;

  if not AOnlyMozilla then begin

  if FindFieldInHeaders('FROM:',AMessage,s) then
    AHeader.From:=DecodeLine7bit(GetFieldValueFromLine('FROM:',s));

  if FindFieldInHeaders('SENDER:',AMessage,s) then
    AHeader.Sender:=DecodeLine7bit(GetFieldValueFromLine('SENDER:',s));

  if FindFieldInHeaders('MESSAGE-ID:',AMessage,s) then
    AHeader.MessageId:=GetFieldValueFromLine('MESSAGE-ID:',s);

  if FindFieldInHeaders( 'RETURN-PATH:', AMessage, s) then
    AHeader.ReturnPath:=GetFieldValueFromLine( 'RETURN-PATH:', s);

  if FindFieldInHeaders('REPLY-TO:',AMessage, s) then
    AHeader.ReplyTo:=GetFieldValueFromLine('REPLY-TO:',s);

  if FindFieldInHeaders('TO:',AMessage,s) then
    AHeader.SendTo:=DecodeLine7bit(GetFieldValueFromLine('TO:',s));

  if FindFieldInHeaders('CC:',AMessage,s)  then
    AHeader.CC:=DecodeLine7bit(GetFieldValueFromLine('CC:',s));

  if FindFieldInHeaders('DATE:',AMessage,s)  then begin
    AHeader.Date:=GetFieldValueFromLine('DATE:',s);
    try
      AHeader.Timestamp:=MailDateToDateTime(AHeader.Date);
    except
      AHeader.Timestamp:=0;
    end;
  end;

  if FindFieldInHeaders('SUBJECT:',AMessage,s) then
    AHeader.Subject:=DecodeLine7bit(GetFieldValueFromLine('SUBJECT:',s));

  if FindFieldInHeaders('X-PRIORITY:',AMessage,s) then
    AHeader.Priority:=StrToIntDef(GetFieldValueFromLine('X-PRIORITY:',s),0);

  if FindFieldInHeaders('CONTENT-TYPE:',AMessage,s) then
    AHeader.ContentType:=UpperCase(GetFieldValueFromLine('CONTENT-TYPE:',s))
  else
    AHeader.ContentType:='TEXT/PLAIN';

  if FindFieldInHeaders('CONTENT-TRANSFER-ENCODING:',AMessage,s) then
    AHeader.ContentTransferEncoding:=UpperCase(GetFieldValueFromLine('CONTENT-TRANSFER-ENCODING:',s))
  else
    AHeader.ContentTransferEncoding:='7BIT';

  end;

  if FindFieldInHeaders('X-TEAMUP-REVISIONID:',AMessage,s) then
    AHeader.RevisionID:=UpperCase(GetFieldValueFromLine('X-TEAMUP-REVISIONID:',s));
  if FindFieldInHeaders('X-TEAMUP-DOCUMENTID:',AMessage,s) then
    AHeader.RevisionID:=UpperCase(GetFieldValueFromLine('X-TEAMUP-DOCUMENTID:',s));
end;

end.
