{**************************************************************************}
{ Mini HTML rendering engine                                               }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 1999-2015                                         }
{            Email : info@tmssoftware.com                                  }
{            Website : http://www.tmssoftware.com/                         }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AsgHTMLE;

{$I TMSDEFS.INC}
{$DEFINE HILIGHT}
{$DEFINE REMOVEDRAW}
{$DEFINE REMOVEIPOSFROM}

interface

uses
  Windows, Graphics, PictureContainer, Classes, Controls, ComCtrls, Dialogs
  , ImgList, Messages, AdvXPVS, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

var
  IsWinXP: Boolean;
  ASG_ATTR_DELIM: char = '"';
  ASG_KEEP_LINEBREAKS: boolean = true;

function HiLight(s,h,tag:string;DoCase,FullText:boolean):string;
function UnHiLight(s,tag:string):string;
function FixMarkup(su:string; SpecialChars: boolean = true): string;
function UnFixMarkup(su:string; SpecialChars: boolean = true): string;
function FixNonBreaking(su:string): string;
function HTMLStrip(s:string):string;
function HTMLDrawEx(Canvas:TCanvas; s:string; fr:TRect;
                    FImages: TCustomImageList;
                    XPos,YPos,FocusLink,HoverLink,ShadowOffset: Integer;
                    CheckHotSpot,CheckHeight,Print,Selected,Blink,HoverStyle,WordWrap,Down: Boolean;
                    DownID: string; ResFactor:Double;
                    URLColor,HoverColor,HoverFontColor,ShadowColor:TColor;
                    var AnchorVal,StripVal,FocusAnchor,AnchorHint: string;
                    var XSize,YSize,HyperLinks,MouseLink: Integer;
                    var HoverRect,ControlRect:TRect;var CID,CV,CT: string;
                    ic: THTMLPictureCache; pc: TPictureContainer; WinHandle: THandle; BidiMode : TBidiMode = bdLeftToRight): Boolean;

function GetControlValue(HTML,ControlID:string;var ControlValue:String): Boolean;
function GetControlProp(HTML,ControlID:string): string;
function GetControlMaxLen(HTML,ControlID:string): integer;
function SetControlValue(var HTML:string;ControlID,ControlValue:string): Boolean;
function ClearRadioControls(HTML: string): string;
function GetNextControlID(HTML:string; ControlID: string): string;
function HasHTMLControl(HTML: string): boolean;
procedure PrintBitmap(Canvas:  TCanvas; DestRect:  TRect;  Bitmap:  TBitmap);

procedure SetHighLightColors(AColor, ATextColor: TColor);


implementation

uses
  ShellAPI, SysUtils, CommCtrl;

const
  NumSpecialChar = 51;

type
  TColorentry = record
    sc:  pchar;
    tc:  tcolor;
  end;

const // color names with the cl removed
  // could easily be extended
  colorTable: array[0..39] of TColorEntry = ( (sc:'3ddkshadow';tc:clgraytext),
  (sc:'3dlight';tc:cl3dlight),
  (sc:'activeborder';tc:clActiveborder),
  (sc:'activecaption';tc:clActivecaption),
  (sc:'appworkspace';tc:clAppworkspace),
  (sc:'aqua';tc:claqua),
  (sc:'background';tc:clBackground),
  (sc:'black';tc:clBlack),
  (sc:'blue';tc:clBlue),
  (sc:'btnface';tc:clBtnface),
  (sc:'btnhighlight';tc:clBtnhighlight),
  (sc:'btnshadow';tc:clBtnshadow),
  (sc:'btntext';tc:clBtntext),
  (sc:'captiontext';tc:clCaptiontext),
  (sc:'fuchsia';tc:clFuchsia),
  (sc:'gray';tc:clGray),
  (sc:'graytext';tc:clGraytext),
  (sc:'green';tc:clGreen),
  (sc:'highlight';tc:clHighlight),
  (sc:'highlighttext';tc:clHighlighttext),
  (sc:'inactiveborder';tc:clInactiveborder),
  (sc:'inactivecaption';tc:clInactivecaption),
  (sc:'inactivecaptiontext';tc:clInactivecaptiontext),
  (sc:'infobk';tc:clInfobk),
  (sc:'infotext';tc:clInfotext),
  (sc:'lime';tc:clLime),
  (sc:'maroon';tc:clMaroon),
  (sc:'menu';tc:clMenu),
  (sc:'menutext';tc:clMenutext),
  (sc:'navy';tc:clNavy),
  (sc:'olive';tc:clOlive),
  (sc:'purple';tc:clPurple),
  (sc:'red';tc:clRed),
  (sc:'silver';tc:clSilver),
  (sc:'teal';tc:clTeal),
  (sc:'white';tc:clWhite),
  (sc:'window';tc:clWindow),
  (sc:'windowframe';tc:clWindowframe),
  (sc:'windowtext';tc:clWindowtext),
  (sc:'yellow';tc:clYellow));


var
  HTMLHighlightColor: TColor = clHighlight;
  HTMLHighlightTextColor: TColor = clHighlightText;

  HTMLEncodedChar : array[1..55] of ansistring = ('é','è','ë','ê',
                                             'ó','ò','ö','ô',
                                             'í','ì','ï','î',
                                             'ú','ù','ü','û',
                                             'á','à','ä','â',
                                             'É','È','Ë','Ê',
                                             'Ó','Ò','Ö','Ô',
                                             'Í','Ì','Ï','Î',
                                             'Ú','Ù','Ü','Û',
                                             'Á','À','Ä','Â',
                                             'ç','Ç','ø','Ø',
                                             'å','Å','©','®',
                                             '€','«','»','ã','Ã','õ','Õ');

  HTMLSpecialChar : array[1..55] of ansistring = ('&eacute;','&egrave;','&euml;','&ecirc;',
                                             '&oacute;','&ograve;','&ouml;','&ocirc;',
                                             '&iacute;','&igrave;','&iuml;','&icirc;',
                                             '&uacute;','&ugrave;','&uuml;','&ucirc;',
                                             '&aacute;','&agrave;','&auml;','&acirc;',
                                             '&Eacute;','&Egrave;','&Euml;','&Ecirc;',
                                             '&Oacute;','&Ograve;','&Ouml;','&Ocirc;',
                                             '&Iacute;','&Igrave;','&Iuml;','&Icirc;',
                                             '&Uacute;','&Ugrave;','&Uuml;','&Ucirc;',
                                             '&Aacute;','&Agrave;','&Auml;','&Acirc;',
                                             '&ccedil;','&Ccedil;','&oslash;','&Oslash;',
                                             '&aring;','&Aring;','&copy;','&reg;',
                                             '&euro;','&laquo;','&raquo;','&atilde;','&Atilde;','&otilde;','&Otilde');

procedure SetHighLightColors(AColor, ATextColor: TColor);
begin
  HTMLHighlightColor := AColor;
  HTMLHighlightTextColor := ATextColor;
end;

procedure PrintBitmap(Canvas:  TCanvas; DestRect:  TRect;  Bitmap:  TBitmap);
var
  BitmapHeader:  pBitmapInfo;
  BitmapImage :  POINTER;
  HeaderSize  :  DWORD;
  ImageSize   :  DWORD;

begin
  GetDIBSizes(Bitmap.Handle, HeaderSize, ImageSize);
  GetMem(BitmapHeader, HeaderSize);
  GetMem(BitmapImage,  ImageSize);
  try
    GetDIB(Bitmap.Handle, Bitmap.Palette, BitmapHeader^, BitmapImage^);
    StretchDIBits(Canvas.Handle,
                  DestRect.Left, DestRect.Top,     // Destination Origin
                  DestRect.Right  - DestRect.Left, // Destination Width
                  DestRect.Bottom - DestRect.Top,  // Destination Height
                  0, 0,                            // Source Origin
                  Bitmap.Width, Bitmap.Height,     // Source Width & Height
                  BitmapImage,
                  TBitmapInfo(BitmapHeader^),
                  DIB_RGB_COLORS,
                  SRCCOPY)
  finally
    FreeMem(BitmapHeader);
    FreeMem(BitmapImage)
  end;
end;

function DirExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));

  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function SysImage(Canvas: TCanvas;x,y:Integer;APath:string;large,draw,print:boolean;resfactor:double):TPoint;
var
  SFI: TSHFileInfo;
  i,Err: Integer;
  imglsthandle: THandle;
  rx,ry: Integer;
  bmp:TBitmap;
  r: TRect;
begin
  Val(APath,i,Err);

  if (APath <> '') and (Err <> 0) then
  begin
    if FileExists(APath) or DirExists(APath) then
    // If the file or directory exists, just let Windows figure out it's attrs.
      SHGetFileInfo(PChar(APath), 0, SFI, SizeOf(TSHFileInfo),
                    SHGFI_SYSICONINDEX {or OPEN_FLAG[Open] or SELECTED_FLAG[Selected]})
    else
    // File doesn't exist, so Windows doesn't know what to do with it.  We have
    // to tell it by passing the attributes we want, and specifying the
    // SHGFI_USEFILEATTRIBUTES flag so that the function knows to use them.
      SHGetFileInfo(PChar(APath), 0, SFI, SizeOf(TSHFileInfo),
                    SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES {or OPEN_FLAG[Open] or SELECTED_FLAG[Selected]});
    i := SFI.iIcon;
  end;

  if Large then
    imglsthandle := SHGetFileInfo('', 0, SFI, SizeOf(SFI),
                            SHGFI_SYSICONINDEX or SHGFI_LARGEICON)
  else
    imglsthandle := SHGetFileInfo('', 0, SFI, SizeOf(SFI),
                            SHGFI_SYSICONINDEX or SHGFI_SMALLICON);


  ImageList_GetIconSize(imglsthandle,rx,ry);
  Result := Point(rx,ry);

  if Draw and not Print then
    ImageList_Draw(imglsthandle,i,Canvas.handle,x,y, ILD_TRANSPARENT);

  if Draw and Print then
  begin
    bmp := TBitmap.Create;
    try
      bmp.Width := rx;
      bmp.Height := ry;
      ImageList_Draw(imglsthandle,i,bmp.Canvas.handle,0,0,ILD_NORMAL);
      r.left := x;
      r.top := y;
      r.right := x + Round(rx * ResFactor);
      r.bottom := y + Round(ry * ResFactor);
      PrintBitmap(Canvas,r,bmp);
    finally
      bmp.Free;
    end;
  end;
end;

(*

function Text2Color(s:string):tcolor;
begin
  Result := clBlack;

  if (s = 'clred') then Result := clred else
  if (s = 'clblack') then Result := clblack else
  if (s = 'clblue') then Result := clblue else
  if (s = 'clgreen') then Result := clgreen else
  if (s = 'claqua') then Result := claqua else
  if (s = 'clyellow') then Result := clyellow else
  if (s = 'clfuchsia') then Result := clfuchsia else
  if (s = 'clwhite') then Result := clwhite else
  if (s = 'cllime') then Result := cllime else
  if (s = 'clsilver') then Result := clsilver else
  if (s = 'clgray') then Result := clgray else
  if (s = 'clolive') then Result := clolive else
  if (s = 'clnavy') then Result := clnavy else
  if (s = 'clpurple') then Result := clpurple else
  if (s = 'clteal') then Result := clteal else
  if (s = 'clmaroon') then Result := clmaroon;

  if Result <> clBlack then Exit;

  if (s = 'clbackground') then Result := clbackground else
  if (s = 'clactivecaption') then Result := clactivecaption else
  if (s = 'clinactivecaption') then Result := clinactivecaption else
  if (s = 'clmenu') then Result := clmenu else
  if (s = 'clwindow') then Result := clwindow else
  if (s = 'clwindowframe') then Result := clwindowframe else
  if (s = 'clmenutext') then Result := clmenutext else
  if (s = 'clwindowtext') then Result := clwindowtext else
  if (s = 'clcaptiontext') then Result := clcaptiontext else
  if (s = 'clactiveborder') then Result := clactiveborder else
  if (s = 'clinactiveborder') then Result := clinactiveborder else
  if (s = 'clappworkspace') then Result := clappworkspace else
  if (s = 'clhighlight') then Result := clhighlight else
  if (s = 'clhighlighttext') then Result := clhighlighttext else
  if (s = 'clbtnface') then Result := clbtnface else
  if (s = 'clbtnshadow') then Result := clbtnshadow else
  if (s = 'clgraytext') then Result := clgraytext else
  if (s = 'clbtntext') then Result := clbtntext else
  if (s = 'clinactivecaptiontext') then Result := clinactivecaptiontext else
  if (s = 'clbtnhighlight') then Result := clbtnhighlight else
  if (s = 'cl3ddkshadow') then Result := clgraytext else
  if (s = 'cl3dlight') then Result := cl3dlight else
  if (s = 'clinfotext') then Result := clinfotext else
  if (s = 'clinfobk') then Result := clinfobk;
end;
*)

function Str2hexbyte(src: string; ndx: integer): integer; var
  n:    integer;
  i:    integer;
begin
  Result := 0;
  if not (length(src) > ndx) then
    Exit; // or raise an exception src is not big enough to hold a hex byte
  for i := 0 to 1 do
  begin
    n := ord(upcase(src[ndx+i])) - ord('0');
    if n > 9 then
      n := n - 7;
    if (n < 0) or (n > 15) then
    begin
      Result := 0;  // invalid hex digit
      Exit;         // might be better to raise an exception as alternate action
    end;            // so programmer would know the entry was wrong
    Result := Result shl 4 or n;
  end;
end { Str2HexByte };


function Text2Color(s:string):tcolor;
var
  r,g,b: Integer;
  top, bot, cur:  integer;
  key:  pchar;
begin
  if s[1] = '#' then
  begin
    r := Str2hexbyte(s,2);
    g := Str2hexbyte(s,4) shl 8;
    b := Str2Hexbyte(s,6) shl 16;
    Result := TColor(b + g + r);
    exit;
  end;
  Result := clBlack; // 40 colors
  if pos('cl',s) <> 1 then
    Exit;   // all colors start with cl

  // do a binary search for chosen color -- max 6 comparisons
  key := pchar(s)+2; // skip the cl in front of color name
  bot := 0;
  top := high(colortable);
  while bot <= top do
  begin
    cur := (bot + top) div 2;
    if strComp(colortable[cur].sc,key) < 0 then
      bot := cur + 1
    else
      top := cur - 1;
  end;
  // one more comparison to verify exact match
  if strcomp(colortable[bot].sc,key) = 0 then
    result := colortable[bot].tc;
end { TextToColor };


function HexVal(s:string): Integer;
var
  i,j: Integer;
begin
  if Length(s) < 2 then
  begin
    Result := 0;
    Exit;
  end;

  if s[1] >= 'A' then
    i := ord(s[1]) - ord('A') + 10
  else
    i := ord(s[1]) - ord('0');

  if s[2] >= 'A' then
    j := ord(s[2]) - ord('A') + 10
  else
    j := ord(s[2]) - ord('0');

  Result := i shl 4 + j;
end;

function Hex2Color(s:string): TColor;
var
  r,g,b: Integer;
begin
  r := Hexval(Copy(s,2,2));
  g := Hexval(Copy(s,4,2)) shl 8;
  b := Hexval(Copy(s,6,2)) shl 16;
  Result := TColor(b + g + r);
end;

function IPos(su,s:string):Integer;
begin
  Result := Pos(UpperCase(su),UpperCase(s));
end;

function IStrToInt(s:string):Integer;
var
  Err,Res: Integer;
begin
  Val(s,Res,Err);
  Result := Res;
end;

function DBTagStrip(s:string):string;
var
  i,j: Integer;
begin
  repeat
    i := Pos('<#',s);
    if i > 0 then
      begin
        Result := Copy(s,1,i - 1);
        Delete(s,1,i);
        j := Pos('>',s);
        if j > 0 then
          Delete(s,j,1);
        Result := Result + s;
        s := Result;
      end
    else
      Result := s;
  until (i <= 0); 
end;

function CRLFStrip(s: string; break:boolean):string;
var
  i,ls: Integer;
  lc,nc: char;

begin
  Result := '';
  ls := Length(s);

  for i := 1 to ls do
  begin
    if i > 1 then
      lc := s[i - 1]
    else
      lc := #0;

    if i + 1 < ls then
      nc := s[i + 1]
    else
      nc := #0;

    if not ( (s[i] = #13) or (s[i] = #10) ) then
      Result := Result + s[i]
    else
      if (s[i] = #13) then
        if break then
          Result := Result + '<BR>'
        else
        begin
          if (lc <> '>') or (nc <> '<') then
            Result := Result + ' ';
        end;
  end;
end;

function VarPos(su,s:string;var Res:Integer):Integer;
begin
  Res := Pos(su,s);
  Result := Res;
end;

function VarIPos(su,s:string;var Res:Integer):Integer;
begin
  Res := Pos(su,Uppercase(s));
  Result := Res;
end;


function TagReplaceString(const Srch,Repl:string;var Dest:string):Boolean;
var
  i: Integer;
begin
  i := IPos(srch,dest);
  if i > 0 then
  begin
    Result := True;
    Delete(Dest,i,Length(Srch));
    Dest := Copy(Dest,1,i-1) + Repl + Copy(Dest,i,Length(Dest));
  end
  else
    Result := False;
end;


function UnFixMarkup(su:string;SpecialChars: boolean = true):string;
var
  i: integer;
begin
  while Pos('&lt;',su) > 0 do
  begin
    TagReplacestring('&lt;','<',su);
  end;

  while Pos('&gt;',su) > 0 do
  begin
    TagReplacestring('&gt;','>',su);
  end;

  while Pos('&amp;',su) > 0 do
  begin
    TagReplacestring('&amp;','&',su);
  end;

  while Pos('&quot;',su) > 0 do
  begin
    TagReplacestring('&quot;','"',su);
  end;

  if SpecialChars then
  begin
    for i := 1 to NumSpecialChar do
    begin
      while Pos(string(HTMLSpecialChar[i]),su) > 0 do
      begin
        TagReplacestring(string(HTMLSpecialChar[i]),string(HTMLEncodedChar[i]),su);
      end;
    end;
  end;

  Result := su;
end;

function FixMarkup(su:string;SpecialChars: boolean = true): string;
var
  i: integer;
begin
  while Pos('&',su) > 0 do
  begin
    TagReplacestring('&','*amp;',su);
  end;

  while Pos('*amp;',su) > 0 do
  begin
    TagReplacestring('*amp;','&amp;',su);
  end;

  while Pos('"',su) > 0 do
  begin
    TagReplacestring('"','&quot;',su);
  end;

  while Pos('<',su) > 0 do
  begin
    TagReplacestring('<','&lt;',su);
  end;

  while Pos('>',su) > 0 do
  begin
    TagReplacestring('>','&gt;',su);
  end;

  if SpecialChars then
  begin
    for i := 1 to NumSpecialChar do
    begin
      while Pos(string(HTMLEncodedChar[i]),su) > 0 do
      begin
        TagReplacestring(string(HTMLEncodedChar[i]),string(HTMLSpecialChar[i]),su);
      end;
    end;
  end;

  Result := su;
end;

function FixNonBreaking(su: string): string;
begin
  while Pos(' ',su) > 0 do
  begin
    TagReplacestring(' ','&nbsp;',su);
  end;
  Result := su;
end;

procedure ParseControl(Tag: string; var ControlType,ControlID,ControlValue,ControlWidth,ControlHeight,ControlProp,ControlLen:string);
var
  Prop: string;
  vp: integer;
begin
  ControlType := '';
  ControlWidth := '';
  ControlHeight := '';
  ControlValue := '';
  ControlID := '';
  ControlProp := '';
  ControlLen := '';

  if VarIPos('TYPE=',Tag,vp) > 0 then
  begin
    Prop := Copy(Tag,vp + 1,Length(Tag));
    Prop := Copy(Prop,Pos(ASG_ATTR_DELIM,Prop) + 1,Length(Prop));
    Prop := Copy(Prop,1,Pos(ASG_ATTR_DELIM,Prop) - 1);
    ControlType := Uppercase(Prop);
  end;

  if VarIPos('WIDTH=',Tag,vp) > 0 then
  begin
    Prop := Copy(Tag,vp + 1,Length(Tag));
    Prop := Copy(Prop,Pos(ASG_ATTR_DELIM,Prop) + 1,Length(Prop));
    Prop := Copy(Prop,1,Pos(ASG_ATTR_DELIM,Prop) - 1);
    ControlWidth := Prop;
  end;

  if VarIPos('HEIGHT=',Tag,vp) > 0 then
  begin
    Prop := Copy(Tag,vp + 1,Length(Tag));
    Prop := Copy(Prop,Pos(ASG_ATTR_DELIM,Prop) + 1,Length(Prop));
    Prop := Copy(Prop,1,Pos(ASG_ATTR_DELIM,Prop) - 1);
    ControlHeight := Prop;
  end;

  if VarIPos('ID=',Tag,vp) > 0 then
  begin
    Prop := Copy(Tag,vp + 1,Length(Tag));
    Prop := Copy(Prop,Pos(ASG_ATTR_DELIM,Prop) + 1,Length(Prop));
    Prop := Copy(Prop,1,Pos(ASG_ATTR_DELIM,Prop) - 1);
    ControlID := Prop;
  end;

  if VarIPos('VALUE=',Tag,vp) > 0 then
  begin
    Prop := Copy(Tag,vp + 1,Length(Tag));
    Prop := Copy(Prop,Pos(ASG_ATTR_DELIM,Prop) + 1,Length(Prop));
    Prop := Copy(Prop,1,Pos(ASG_ATTR_DELIM,Prop) - 1);
    ControlValue := UnFixMarkup(Prop);
  end;

  if VarIPos('PROP=',Tag,vp) > 0 then
  begin
    Prop := Copy(Tag,vp + 1,Length(Tag));
    Prop := Copy(Prop,Pos(ASG_ATTR_DELIM,Prop) + 1,Length(Prop));
    Prop := Copy(Prop,1,Pos(ASG_ATTR_DELIM,Prop) - 1);
    ControlProp := UnFixMarkup(Prop);
  end;

  if VarIPos('MAXLEN=',Tag,vp) > 0 then
  begin
    Prop := Copy(Tag,vp + 1,Length(Tag));
    Prop := Copy(Prop,Pos(ASG_ATTR_DELIM,Prop) + 1,Length(Prop));
    Prop := Copy(Prop,1,Pos(ASG_ATTR_DELIM,Prop) - 1);
    ControlLen := Prop;
  end;
end;

function HasHTMLControl(HTML: string): boolean;
var
  lp: Integer;
begin
  Result := VarIPos('<CONTROL', HTML,lp) > 0;
end;

function GetNextControlID(HTML, ControlID:string): string;
var
  lp: Integer;
  Tag,CType,CID,CV,CW,CH,CP,CL: string;
  flg: boolean;
begin
  Result := '';
  flg := ControlID = '';

  while VarIPos('<CONTROL ',html,lp) > 0 do
  begin
    Delete(html,1,lp);
    Tag := Copy(html,1,Pos('>',html));
    ParseControl(Tag,CType,CID,CV,CW,CH,CP,CL);

    if flg and (CType <> 'BUTTON') and (CType <> 'CHECK') and (CType <> 'RADIO') then
    begin
      Result := CID;
      Exit;
    end;

    if (ControlID = CID) then
      flg := true;
  end;
end;

function ClearRadioControls(HTML: string): string;
var
  lp: Integer;
  Tag,CType,CID,CV,CW,CH,CP,CL: string;
  sl: TStringList;
  s: string;
begin
  Result := '';

  s := HTML;

  sl := TStringList.Create;
  while VarIPos('<CONTROL ',html,lp) > 0 do
  begin
    Delete(html,1,lp);
    Tag := Copy(html,1,Pos('>',html));
    ParseControl(Tag,CType,CID,CV,CW,CH,CP,CL);

    if CType = 'RADIO' then
      sl.Add(CID);
  end;

  for lp := 0 to sl.Count - 1 do
  begin
    SetControlValue(s, sl.Strings[lp], 'FALSE');
  end;

  sl.Free;
  Result := s;
end;

function GetControlValue(HTML,ControlID:string;var ControlValue:String): Boolean;
var
  lp: Integer;
  Tag,CType,CID,CV,CW,CH,CP,CL: string;
begin
  Result := False;
  while VarIPos('<CONTROL ',html,lp) > 0 do
  begin
    Delete(html,1,lp);
    Tag := Copy(html,1,Pos('>',html));
    ParseControl(Tag,CType,CID,CV,CW,CH,CP,CL);
    if (ControlID = CID) then
    begin
      ControlValue := CV;
      Result := True;
      Exit;
    end;
  end;
end;

function GetControlProp(HTML,ControlID:string): string;
var
  lp: Integer;
  Tag,CType,CID,CV,CW,CH,CP,CL: string;
begin
  Result := '';
  while VarIPos('<CONTROL ',html,lp) > 0 do
  begin
    Delete(html,1,lp);
    Tag := Copy(html,1,Pos('>',html));
    ParseControl(Tag,CType,CID,CV,CW,CH,CP,CL);
    if (ControlID = CID) then
    begin
      Result := CP;
      Exit;
    end;
  end;
end;

function GetControlMaxLen(HTML,ControlID:string): integer;
var
  lp,e: Integer;
  Tag,CType,CID,CV,CW,CH,CP,CL: string;
begin
  Result := 0;
  while VarIPos('<CONTROL ',html,lp) > 0 do
  begin
    Delete(html,1,lp);
    Tag := Copy(html,1,Pos('>',html));
    ParseControl(Tag,CType,CID,CV,CW,CH,CP,CL);

    if (ControlID = CID) then
    begin
      val(CL,Result,e);
      Exit;
    end;
  end;
end;

function SetControlValue(var HTML:string;ControlID,ControlValue:string): Boolean;
var
  lp: Integer;
  Tag,Temp,CType,CID,CV,CW,CH,CP,CL: string;
begin
  Result := False;
  Temp := '';
  ControlValue := FixMarkup(ControlValue);
  while VarIPos('<CONTROL ',html,lp) > 0 do
  begin
    Temp := Temp + Copy(html,1,lp);
    Delete(html,1,lp);
    Tag := Copy(html,1,Pos('>',html));
    ParseControl(Tag,CType,CID,CV,CW,CH,CP,CL);
    if (ControlID = CID) then
    begin
      Temp := Temp + 'CONTROL ID='+ASG_ATTR_DELIM+ControlID+ASG_ATTR_DELIM+' VALUE='+ASG_ATTR_DELIM+ControlValue+ASG_ATTR_DELIM+' WIDTH='+ASG_ATTR_DELIM+CW+ASG_ATTR_DELIM+' TYPE=' + ASG_ATTR_DELIM +CType+ASG_ATTR_DELIM;
      if (CP <> '') then
        Temp := Temp + ' PROP='+ASG_ATTR_DELIM+FixMarkup(CP)+ASG_ATTR_DELIM;
      if (CL <> '') then
        Temp := Temp + ' MAXLEN='+ASG_ATTR_DELIM+CL+ASG_ATTR_DELIM;

      Temp := Temp + '>';
      html := Temp + Copy(html,pos('>',html)+1,Length(html));
      Result := True;
      Exit;
    end;
  end;
end;

function ConvertToPassword(s:string):string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
    Result := Result + '*';
end;

{$WARNINGS OFF}
function HTMLDrawEx(Canvas:TCanvas; s:string; fr:TRect;
                    FImages: TCustomImageList;
                    XPos,YPos,FocusLink,HoverLink,ShadowOffset: Integer;
                    CheckHotSpot,CheckHeight,Print,Selected,Blink,HoverStyle,WordWrap,Down: Boolean;
                    DownID: string; ResFactor:Double;
                    URLColor,HoverColor,HoverFontColor,ShadowColor:TColor;
                    var AnchorVal,StripVal,FocusAnchor,AnchorHint: string;
                    var XSize,YSize,HyperLinks,MouseLink: Integer;
                    var HoverRect,ControlRect:TRect;var CID,CV,CT:string;
                    ic: THTMLPictureCache; pc: TPictureContainer; WinHandle: THandle; BidiMode: TBidiMode = bdLeftToRight): Boolean;
var
  su: string;
  r,dr,hr,rr,er: TRect;
  htmlwidth,htmlheight,txtheight,ch: Integer;
  Align: TAlignment;
  PIndent: Integer;
  OldFont: TFont;
  CalcFont: TFont;
  DrawFont: TFont;
  OldCalcFont: TFont;
  OldDrawFont: TFont;
  Hotspot, ImageHotspot: Boolean;
  Anchor,OldAnchor,MouseInAnchor,Error: Boolean;
  bgcolor,paracolor,hvrcolor,hvrfntcolor,pencolor,blnkcolor,hifcol,hibcol: TColor;
  LastAnchor,OldAnchorVal,LastAnchorHint, OldAnchorHint: string;
  IMGSize: TPoint;
  isSup,isSub,isPara,isShad,isHi: Boolean;
  subh,suph,imgali,hlcount,licount: Integer;
  hrgn,holdfont: THandle;
  ListIndex,OListIndex: Integer;
  dtp: TDrawTextParams;
  Invisible: Boolean;
  FoundTag: Boolean;
  {new for editing}
  nnFit: Integer;
  nnSize: TSize;
  inspoint: Integer;
  AltImg,ImgIdx,OldImgIdx: Integer;
  DrawStyle: DWord;
  HTHeme: THandle;
  FHot: Boolean;
  UseWinXP: Boolean;
  OrgPenColor: TColor;
  OrgPenWidth: Integer;
  OLIST: boolean;
  OListIndexStr: string;

  procedure StartRotated(Canvas:TCanvas;Angle: Integer);
  var
    LFont:TLogFont;
  begin
    GetObject(Canvas.Font.Handle,SizeOf(LFont),Addr(LFont));

    LFont.lfEscapement := Angle * 10;
    LFont.lfOrientation := Angle * 10;
    hOldFont := SelectObject(Canvas.Handle,CreateFontIndirect(LFont));
  end;

  procedure EndRotated(Canvas:TCanvas);
  begin
    DeleteObject(SelectObject(Canvas.Handle,hOldFont));
  end;

  function HTMLDrawLine(Canvas: TCanvas;var s:string;r: TRect;Calc:Boolean;
                        var w,h,subh,suph,imgali:Integer;var Align:TAlignment; var PIndent: Integer;
                        XPos,YPos:Integer;var Hotspot,ImageHotSpot:Boolean): string;
  var
    su,Res,TagProp,Prop,Tagp,LineText:string;
    cr,ir: TRect;
    linebreak,imgbreak,linkbreak: Boolean;
    th,sw,indent,err,bmpx,bmpy,imgw,imgh,spc: Integer;
    TagPos,SpacePos,o,l: Integer;
    bmp: TGraphic;
    ABitmap: TBitmap;
    NewColor: TColor;
    TagWidth,TagHeight,WordLen,WordLenEx,WordWidth: Integer;
    TagChar: Char;
    LengthFits: Boolean;
    ControlType,ControlWidth,ControlHeight,ControlID,ControlValue,ControlProp,ControlLen: string;

  begin
    Result := '';
    LineText := '';
    r.Bottom := r.Bottom - Subh;

    w := 0;
    sw := 0;

    LineBreak := False;
    ImgBreak := False;
    LinkBreak := False;
    HotSpot := False;
    ImageHotSpot := False;
    cr := r;
    res := '';

    if isPara and not Calc then
    begin
      Pencolor := Canvas.Pen.Color;
      Canvas.Pen.color := Canvas.Brush.Color;
      Canvas.Rectangle(fr.Left,r.Top,fr.Right,r.Top + h);
    end;

    while (Length(s) > 0) and not LineBreak and not ImgBreak do
    begin
      // get next word or till next HTML tag
      TagPos := Pos('<',s);

      if WordWrap then
        SpacePos := Pos(' ',s)
      else
        SpacePos := 0;

      if (Tagpos > 0) and ((SpacePos > TagPos) or (SpacePos = 0)) then
      begin
        su := Copy(s,1,TagPos - 1);
      end
      else
      begin
        if SpacePos > 0 then
          su := Copy(s,1,SpacePos)
        else
          su := s;
      end;

      {$IFDEF TMSDEBUG}
      DbgMsg(su+ '.');
      {$ENDIF}

      WordLen := Length(su);

      while Pos('&nbsp;',su) > 0 do
      begin
        TagReplacestring('&nbsp;',' ',su);
      end;

      while Pos('&lt;',su) > 0 do
      begin
        TagReplacestring('&lt;','<',su);
      end;

      while Pos('&gt;',su) > 0 do
      begin
        TagReplacestring('&gt;','>',su);
      end;

      for spc := 1 to NumSpecialChar do
      begin
        while Pos(string(HTMLSpecialChar[spc]),su) > 0 do
        begin
          TagReplacestring(string(HTMLSpecialChar[spc]),string(HTMLEncodedChar[spc]),su);
        end;
      end;

      WordLenEx := Length(su);
      WordWidth := 0;

      if WordLen > 0 then
      begin
        th := Canvas.TextHeight(su);

        if isSub and (subh < (th shr 2)) then subh := th shr 2;
        if isSup and (suph < (th shr 2)) then suph := th shr 2;

        if th > h then
          h := th;

        StripVal := StripVal + su;

        if Invisible then
          Delete(s,1,WordLen);

        if not Invisible then
        begin
          // draw mode
          if not Calc then
          begin
            if isSup then
              cr.Bottom := cr.Bottom - suph;
            if isSub then
              cr.Bottom := cr.Bottom + subh;

            cr.Bottom := cr.Bottom - imgali;

            if isShad then
            begin
              OffsetRect(cr,ShadowOffset,ShadowOffset);
              NewColor := Canvas.Font.Color;
              Canvas.Font.Color := ShadowColor;
              DrawTextEx(Canvas.Handle,PChar(su),WordLenEx,cr,DrawStyle,nil);
              Offsetrect(cr,-ShadowOffset,-ShadowOffset);
              Canvas.Font.Color := NewColor;
            end;

            err := cr.Right;

            DrawTextEx(Canvas.Handle,PChar(su),WordLenEx,cr,DrawStyle,nil);
            DrawTextEx(Canvas.Handle,PChar(su),WordLenEx,cr,DrawStyle or DT_CALCRECT,nil);

            if Anchor and (Hyperlinks - 1 = FocusLink) then
              FocusAnchor := LastAnchor;

            {$IFDEF TMSDEBUG}
            if Anchor then
              DbgMsg('drawrect for '+anchorval+' = ['+inttostr(cr.Left)+':'+inttostr(cr.Top)+'] ['+inttostr(cr.right)+':'+inttostr(cr.bottom)+'] @ ['+inttostr(xpos)+':'+inttostr(ypos));
            {$ENDIF}

            if Error then
            begin
              Canvas.Pen.Color := clRed;
              Canvas.Pen.Width := 1;

              l := (cr.Left div 2) * 2;
              if (l mod 4)=0 then o := 2 else o := 0;

              Canvas.MoveTo(l,r.Bottom + o - 1);
              while l < cr.Right do
              begin
                if o = 2 then o := 0 else o := 2;
                Canvas.LineTo(l + 2,r.bottom + o - 1);
                Inc(l,2);
              end;
              // if o = 2 then o := 0 else o := 2;
              // Canvas.LineTo(l + 2,r.Bottom + o - 1);
            end;

            // change to Bidimode 
            if DrawStyle and DT_RTLREADING <> DT_RTLREADING then
            begin
              cr.Left := cr.Right;
              cr.Right := r.Right;
            end 
            else 
              cr.Right := err - cr.Right + cr.Left;

            cr.Bottom := r.Bottom;
            cr.Top := r.Top;
          end
        else
          begin
            cr := r; //reinitialized each time !

            DrawTextEx(Canvas.Handle,PChar(su),WordLenEx,cr,DrawStyle or DT_CALCRECT,nil);

            // preparations for editing purposes
            if (ypos > cr.Top) and (ypos < cr.bottom) and (xpos > w) then {scan charpos here}
            begin
              er := rect(w,cr.top,xpos,cr.bottom);

              Fillchar(dtp,sizeof(dtp),0);
              dtp.cbSize := SizeOf(dtp);

              GetTextExtentExPoint(Canvas.Handle,pChar(su),WordLenEx,xpos-w,@nnfit,nil,nnSize);


              {this will get the character pos of the insertion point}
              if nnfit = WordLen then
                InsPoint := InsPoint + WordLen
              else
                InsPoint := InsPoint + nnfit;
            end;
            // end of preparations for editing purposes

            {Calculated text width}
            WordWidth := cr.Right - cr.Left;
            w := w + WordWidth;

            if DrawStyle and DT_RTLREADING = DT_RTLREADING then 
              err := r.Right - XPos - cr.Left
            else
              err := XPos - cr.Left;

            if (XPos - cr.Left >= w - WordWidth) and (err <= w) and Anchor then
            begin
              HotSpot := True;
              if (YPos > cr.Top){ and (YPos < cr.Bottom)} then
              begin
                AnchorVal := LastAnchor;
                AnchorHint := LastAnchorHint;
                MouseInAnchor := True;
              end;
            end;
          end;

          LengthFits := (w < r.Right - r.Left) or (r.Right - r.Left <= WordWidth);

          if not LengthFits and
            ((Length(LineText) > 0) and (LineText[Length(LineText)] <> ' ')) then
            LengthFits := True;

          LineText := LineText + su;

          if LengthFits or not WordWrap then
          begin
            Res := Res + Copy(s,1,WordLen);
            if not LengthFits and Calc and (LineText <> su) then
              s := '';
            Delete(s,1,WordLen);
            if Length(su) >= WordLen then
            begin
              if su[WordLen] = ' ' then
                sw := Canvas.TextWidth(' ')
              else
                sw := 0;
            end
            else
              sw := 0;
          end
          else
          begin
            LineBreak := True;
            w := w - WordWidth;
          end;
        end;
      end;

      TagPos := Pos('<',s);

      if (TagPos = 1) and (Length(s) <= 2) then
        s := '';

      if not LineBreak and (TagPos = 1) and (Length(s) > 2) then
      begin
        if (s[2] = '/') and (Length(s) > 3) then
        begin
          case UpCase(s[3]) of
          'A':begin
                if (not HoverStyle or (Hoverlink = Hyperlinks)) and not Calc then
                begin
                  Canvas.Font.Style := Canvas.Font.Style - [fsUnderline];
                  if Hovercolor <> clNone then
                  begin
                    Canvas.Brush.Color := HvrColor;
                    if HvrColor = clNone then
                      Canvas.Brush.Style := bsClear;
                  end;
                  if HoverFontColor <> clNone then
                    Canvas.Font.Color := HoverFontColor;
                end;

                if not Selected then
                  Canvas.Font.Color := Oldfont.Color;

                Anchor := False;

                if MouseInAnchor then
                begin
                  hr.Bottom := r.Bottom;
                  hr.Right := r.Left + w;
                  if r.Top <> hr.Top then
                  begin
                    hr.Left := r.Left;
                    hr.Top := r.Top;
                  end;

                  HoverRect := hr;
                  MouseLink := HyperLinks;
                  {$IFDEF TMSDEBUG}
                  DbgRect('hotspot anchor '+lastanchor,hr);
                  {$ENDIF}
                  MouseInAnchor := False;
                end;

                if Focuslink = Hyperlinks - 1 then
                begin
                  rr.Right := cr.Left;
                  rr.Bottom := cr.Bottom - ImgAli;
                  rr.Top := rr.Bottom - Canvas.TextHeight('gh');
                  InflateRect(rr,1,0);
                  if not Calc then Canvas.DrawFocusRect(rr);
                end;
              end;
          'E':begin
                if not Calc then
                  Error := False;
              end;
          'B':begin
                if s[4] <> '>' then
                  Canvas.Font.Color := OldFont.Color
                else
                  Canvas.Font.Style := Canvas.Font.Style - [fsBold];
              end;
          'S':begin
                TagChar := UpCase(s[4]);

                if (TagChar = 'T') then  // STRIKE
                begin
                  Canvas.Font.Style := Canvas.Font.Style - [fsStrikeOut];
                end
                else
                if (TagChar = 'U') then  // SUB
                begin
                  isSup := False;
                  isSub := False;
                end
                else
                if (TagChar = 'P') then  // SPAN
                begin
                  Canvas.Brush.Color := BGColor;
                  Canvas.Brush.Style := bsClear;
                end
                else
                if (TagChar = 'H') then
                  isShad := False
                else
                  Canvas.Font.Style := Canvas.Font.Style - [fsStrikeOut];
              end;
          'F':begin
                Canvas.Font.Name := OldFont.Name;
                Canvas.Font.Size := OldFont.Size;
                if not Calc and not Selected then
                begin
                  Canvas.Font.Color := OldFont.Color;
                  Canvas.Brush.Color := BGColor;
                  if BGColor = clNone then
                  begin
                    Canvas.Brush.Style := bsClear;
                  end;
                end;
              end;
          'H':begin
                if not Calc and isHi then
                begin
                  Canvas.Font.Color := hifCol;
                  Canvas.Brush.Color := hibCol;
                  if hibCol = clNone then
                    Canvas.Brush.Style := bsClear;
                end;
              end;
          'I':begin
                Canvas.Font.Style := Canvas.Font.Style - [fsItalic];
              end;
          'L':begin
                LineBreak := True;
              end;
          'P':begin
                LineBreak := True;
                isPara := false;
                if not Calc then
                begin
                  Align := taLeftJustify;
                   Canvas.Brush.Color := ParaColor;
                  if ParaColor = clNone then
                    Canvas.Brush.Style := bsClear;
                end;
              end;
          'U':begin
                if (s[4] <> '>') and (ListIndex > 0) then
                  Dec(Listindex)
                else
                  Canvas.Font.Style := Canvas.Font.Style - [fsUnderline];
              end;
          'O':begin
                if (s[4] <> '>') and (ListIndex > 0) then
                begin
                  Dec(Listindex);
                  if not calc then
                    OList := false;
                end;
              end;
          'R':begin
                EndRotated(Canvas);
              end;
          'Z':Invisible := False;
          end;
        end
        else
        begin
          case Upcase(s[2]) of
          'A':begin
                {only do this when at hover position in xpos,ypos}
                if (FocusLink = HyperLinks) and not Calc then
                begin
                  rr.Left := cr.Left;
                  rr.Top := cr.Top;
                end;

                Inc(HyperLinks);
                if (not HoverStyle or (Hoverlink = HyperLinks)) and not Calc then
                begin
                  Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];
                  if (Hovercolor <> clNone) and not Calc then
                  begin
                    HvrColor := Canvas.Brush.Color;

                    if Canvas.Brush.Style = bsClear then
                      HvrColor := clNone;
                    Canvas.Brush.Color := HoverColor;
                  end;

                  if HoverFontColor <> clNone then
                  begin
                    hvrfntcolor := Canvas.Font.Color;
                    Canvas.Font.Color := HoverFontColor;
                  end;
                end;

                if not Selected and ((HoverFontColor = clNone) or (HoverLink <> HyperLinks) or not HoverStyle) then
                  Canvas.Font.Color := URLColor;

                TagProp := Uppercase(Copy(s,3,Pos('>',s) - 1));  // <A href="anchor">

                if (VarPos('HREF',TagProp,TagPos) > 0) then
                begin

                  TagProp := Copy(s,3,Pos('>',s) - 1);


                  Prop := Copy(TagProp,TagPos + 4,Length(TagProp));
                  Prop := Copy(Prop,Pos(ASG_ATTR_DELIM,Prop) + 1,Length(Prop));
                  Prop := Copy(Prop,1,Pos(ASG_ATTR_DELIM,Prop) - 1);
                  LastAnchor := Prop;
                  Anchor := True;
                end;

                TagProp := Uppercase(Copy(s,3,Pos('>',s) - 1));  // <A href="anchor">

                if (VarPos('TITLE',TagProp,TagPos) > 0) then
                begin
                  TagProp := Copy(s,3,Pos('>',s) - 1);  // <A href="anchor">
                  Prop := Copy(TagProp,TagPos + 5,Length(TagProp));
                  Prop := Copy(Prop,Pos(ASG_ATTR_DELIM,Prop) + 1,Length(Prop));
                  Prop := Copy(Prop,1,Pos(ASG_ATTR_DELIM,Prop) - 1);
                  LastAnchorHint := Prop;
                end
                else
                  LastAnchorHint := '';

{
                TagProp := Copy(s,3,Pos('>',s) - 1);  // <A href="anchor">
                Prop := Copy(TagProp,Pos('"',TagProp) + 1,Length(TagProp));
                Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                LastAnchor := Prop;
                Anchor := True;
}

                hr.Left := w;
                hr.Top := r.Top;
              end;
          'B':begin
                TagChar := Upcase(s[3]);
                if TagChar = '>' then  // <B> tag
                  Canvas.Font.Style := Canvas.Font.Style + [fsBold]
                else
                  if TagChar = 'R' then // <BR> tag
                  begin
                    LineBreak := true;
                    StripVal := StripVal + #13;
                  end
                  else
                  begin
                    if TagChar = 'L' then // <BLINK> tag
                    begin
                      if not Blink then Canvas.Font.Color := BlnkColor;
                    end
                    else
                    if TagChar = 'O' then  // <BODY ... >
                    begin
                      Res := Res + Copy(s,1,pos('>',s));
                      TagProp := Uppercase(Copy(s,6,pos('>',s)-1));

                      if (Pos('BACKGROUND',TagProp) > 0) and not Calc then
                      begin
                        Prop := Copy(TagProp,Pos('BACKGROUND',TagProp)+10,Length(TagProp));
                        Prop := Copy(Prop,Pos(ASG_ATTR_DELIM,Prop)+1,Length(prop));
                        Prop := Copy(Prop,1,Pos(ASG_ATTR_DELIM,Prop)-1);

                        bmp := nil;

                        if (Pos(':',Prop) = 0) and Assigned(pc) then
                        begin
                          bmp := pc.FindPicture(Prop) as THTMLPicture;
                        end;

                        if (Pos('://',Prop) > 0) and Assigned(ic) then
                        begin
                          if ic.FindPicture(Prop) = nil then
                          with ic.AddPicture do
                          begin
                            Asynch := False;
                            LoadFromURL(Prop);
                          end;

                          bmp := ic.FindPicture(Prop) as THTMLPicture;
                        end;

                        if bmp <> Nil then
                        begin
                          if not bmp.Empty and (bmp.Width > 0) and (bmp.Height > 0) then
                          begin
                            // do the tiling here
                            bmpy := 0;
                            hrgn := CreateRectRgn(fr.left, fr.top, fr.right,fr.bottom);
                            SelectClipRgn(Canvas.Handle, hrgn);

                            while (bmpy < fr.bottom-fr.top) do
                            begin
                              bmpx := 0;
                              while (bmpx < fr.right - fr.left) do
                              begin
                                Canvas.Draw(fr.left+bmpx,fr.top+bmpy,bmp);
                                bmpx := bmpx + bmp.width;
                              end;
                              bmpy := bmpy + bmp.height;
                            end;

                            SelectClipRgn(Canvas.handle, 0);
                            DeleteObject(hrgn);
                          end;
                        end; //end of bmp <> nil
                    end; //end of background

                      if (Pos('BGCOLOR',TagProp)>0) then
                      begin
                        Prop := Copy(TagProp,Pos('BGCOLOR',TagProp) + 7,Length(TagProp));
                        Prop := Copy(Prop,Pos(ASG_ATTR_DELIM,Prop) + 1,Length(Prop));
                        Prop := Copy(Prop,1,Pos(ASG_ATTR_DELIM,Prop) - 1);
                        if not Calc then
                        begin
                          if Pos('CL',Prop) > 0 then
                            Canvas.Brush.color := Text2Color(AnsiLowerCase(Prop));
                          if Pos('#',Prop) > 0 then
                            Canvas.Brush.color := Hex2Color(Prop);

                          if not Calc then
                          begin
                            BGColor := Canvas.Brush.Color;
                            Pencolor := Canvas.Pen.Color;
                            Canvas.Pen.color := BGColor;
                            Canvas.Rectangle(fr.Left,fr.Top,fr.Right,fr.Bottom);
                            Canvas.Pen.Color := PenColor;
                          end;
                        end;
                      end;
                    end;
                  end;
                end;
          'E':begin
                if not Calc then
                  Error := True;
              end;
          'C':begin
                { control here }
                { <CONTROL type="EDIT" width="125" ID="name" VALUE="" MAXLEN=""> }

                TagProp := Copy(s,9,pos('>',s)-1);
                ParseControl(TagProp,ControlType,ControlID,ControlValue,ControlWidth,ControlHeight,ControlProp,ControlLen);

                ch := 25;

                if ControlHeight <> '' then
                begin
                  val(ControlHeight,ch,err);
                  if err <> 0 then
                    ch := 25;
                end;

                if ControlWidth <> '' then
                begin
                  val(ControlWidth,Indent,err);

                  if err = 0 then
                  begin
                    IMGSize.x := Indent;
                    IMGSize.y := Canvas.TextHeight('gh') + 10; //changed
                    //IMGSize.y := Canvas.TextHeight('gh') + 6;
                  end;

                  //h := Canvas.TextHeight('gh') + 12;

                  // standard Windows control size
                  if (ControlType = 'CHECK') or (ControlType = 'RADIO') then
                    h := 16
                  else
                    h := 23;

                  if (ch > 25) then
                    h := ch - 2;

                  if not Calc then
                  begin
                    if (ControlType = 'EDIT') or (ControlType = 'PASSWORD') or (ControlType = 'MASK') then
                    begin
                      if UseWinXp then
                        Canvas.Pen.Color := $B99D7F
                      else
                        Canvas.Pen.Color := clGray;

                      Canvas.Brush.Color := clWhite;
                      Canvas.Brush.Style := bsSolid;
                      Canvas.Rectangle(cr.Left ,cr.Bottom - h + 4,cr.Left + Indent, cr.Bottom + 2);
                      Canvas.Brush.Style := bsClear;

                      ir := Rect(cr.Left + 2,cr.Bottom - h + 4,cr.Left + Indent, cr.Bottom);

                      if (ControlType = 'PASSWORD') then
                        DrawText(Canvas.Handle,PChar(ConvertToPassword(ControlValue)),Length(ControlValue),ir,DT_LEFT  or DT_VCENTER or DT_SINGLELINE)
                      else
                        DrawText(Canvas.Handle,PChar(ControlValue),Length(ControlValue),ir,DT_LEFT or DT_VCENTER or DT_SINGLELINE);
                    end;

                    if ControlType = 'COMBO' then
                    begin
                      if UseWinXp then
                        Canvas.Pen.Color := $B99D7F
                      else
                        Canvas.Pen.Color := clGray;

                      Canvas.Brush.Color := clWhite;
                      Canvas.Brush.Style := bsSolid;
                      Canvas.Rectangle(cr.Left ,cr.Bottom - h + 4,cr.Left + Indent, cr.Bottom + 2 );

                      Canvas.Brush.Style := bsClear;
                      ir := Rect(cr.Left + 4,cr.Bottom - h + 4,cr.Left + Indent - 18, cr.Bottom);
                      DrawText(Canvas.Handle,PChar(ControlValue),Length(ControlValue),ir,DT_LEFT or DT_SINGLELINE or DT_VCENTER);

                      ir := Rect(cr.Left + Indent - 18,cr.Bottom - h + 5,cr.Left + Indent - 1, cr.Bottom + 1);

                      if UseWinXP then
                      begin
                        FHot := (XPos > cr.Left - 2) and (XPos < cr.Left + IMGSize.x - 2) and
                                (YPos < cr.Bottom - 2) and (YPos > cr.Bottom - IMGSize.y - 2);
                        HTHeme := OpenThemeData(WinHandle,'combobox');

                        if FHot then
                          DrawThemeBackground(HTheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_HOT,@ir,nil)
                        else
                          DrawThemeBackground(HTheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_NORMAL,@ir,nil);

                        CloseThemeData(HTheme);
                      end
                      else
                        DrawFrameControl(Canvas.Handle,ir,DFC_SCROLL,DFCS_SCROLLCOMBOBOX);
                    end;

                    if ControlType = 'CHECK' then
                    begin
                      IMGSize.x := Round(16 * ResFactor);
                      IMGSize.y := IMGSize.x;
                      Indent := IMGSize.x;

                      if UseWinXp then
                        Canvas.Pen.Color := $B99D7F
                      else
                        Canvas.Pen.Color := clGray;

                      Canvas.Brush.Style := bsClear;
                      ir := Rect(cr.Left + 2,cr.Bottom - (IMGSize.y - 1),cr.Left + (ImgSize.X - 1), cr.Bottom);

                      FHot := False;

                      //FHot := (XPos > cr.Left - 2) and (XPos < cr.Left + IMGSize.x - 2) and
                      //        (YPos < cr.Bottom - 2) and (YPos > cr.Bottom - IMGSize.y - 2);

                      if UseWinXP then
                      begin
                        HTHeme := OpenThemeData(WinHandle,'button');

                        if Uppercase(ControlValue) = 'TRUE' then
                        begin
                          if Down and (DownID = ControlID) and FHot then
                            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDPRESSED,@ir,nil)
                          else
                          if FHot then
                            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDHOT,@ir,nil)
                          else
                            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDNORMAL,@ir,nil);
                        end
                        else
                        begin
                          if Down  and (DownID = ControlID) and FHot then
                            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDPRESSED,@ir,nil)
                          else
                          if FHot then
                            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDHOT,@ir,nil)
                          else
                            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDNORMAL,@ir,nil)
                        end;
                        CloseThemeData(HTHeme);
                      end
                      else
                      begin
                        if Uppercase(ControlValue) = 'TRUE' then
                          DrawFrameControl(Canvas.Handle,ir,DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_FLAT or DFCS_CHECKED)
                        else
                          DrawFrameControl(Canvas.Handle,ir,DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_FLAT);
                      end;
                    end;

                    if ControlType = 'RADIO' then
                    begin
                      IMGSize.x := 16;
                      IMGSize.y := 16;
                      Indent := 16;
                      if UseWinXp then
                        Canvas.Pen.Color := $B99D7F
                      else
                        Canvas.Pen.Color := clGray;
                      Canvas.Brush.Style := bsClear;
                      ir := Rect(cr.Left + 2,cr.Bottom - 14,cr.Left + 14, cr.Bottom);

                      FHot := (XPos > cr.Left - 2) and (XPos < cr.Left + IMGSize.x - 2) and
                              (YPos < cr.Bottom - 2) and (YPos > cr.Bottom - IMGSize.y - 2);

                      FHot := false;

                      if UseWinXP then
                      begin
                        HTHeme := OpenThemeData(WinHandle,'button');

                        if Uppercase(ControlValue) = 'TRUE' then
                        begin
                          if Down and (DownID = ControlID) and FHot then
                            DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_CHECKEDPRESSED,@ir,nil)
                          else
                          if FHot then
                            DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_CHECKEDHOT,@ir,nil)
                          else
                            DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_CHECKEDNORMAL,@ir,nil);
                        end
                        else
                        begin
                          if Down and (DownID = ControlID) and FHot then
                            DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_UNCHECKEDPRESSED,@ir,nil)
                          else
                          if FHot then
                            DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_UNCHECKEDHOT,@ir,nil)
                          else
                            DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_UNCHECKEDNORMAL,@ir,nil)
                        end;
                        CloseThemeData(HTHeme);
                      end
                      else
                      begin
                        if Uppercase(ControlValue) = 'TRUE' then
                          DrawFrameControl(Canvas.Handle,ir,DFC_BUTTON, DFCS_BUTTONRADIO or DFCS_FLAT or DFCS_CHECKED)
                        else
                          DrawFrameControl(Canvas.Handle,ir,DFC_BUTTON, DFCS_BUTTONRADIO or DFCS_FLAT);
                      end;
                    end;

                    if ControlType = 'BUTTON' then
                    begin
                      IMGSize.y := 24;
                      if UseWinXp then
                        Canvas.Pen.Color := $B99D7F
                      else
                        Canvas.Pen.Color := clGray;

                      Canvas.Brush.Style := bsClear;

                      ir := Rect(cr.Left + 2,cr.Bottom - ch,cr.Left + Indent -2, cr.Bottom);

                      FHot := (XPos > cr.Left -2) and (XPos < cr.Left + IMGSize.x - 2) and
                         (YPos < cr.Bottom - 2) and (YPos > cr.Bottom - IMGSize.y - 2);

                      if UseWinXP then
                      begin
                        HTHeme := OpenThemeData(WinHandle,'button');

                        if Down and (DownID = ControlID) {and FHot} then
                          DrawThemeBackground(HTheme,Canvas.Handle, BP_PUSHBUTTON,PBS_PRESSED,@ir,nil)
                        else
                        if FHot then
                          DrawThemeBackground(HTheme,Canvas.Handle, BP_PUSHBUTTON,PBS_HOT,@ir,nil)
                        else
                          DrawThemeBackground(HTheme,Canvas.Handle, BP_PUSHBUTTON,PBS_NORMAL,@ir,nil);

                        CloseThemeData(HTHeme);

                      end
                      else
                      begin
                        if FHot and Down and (DownID = ControlID) then
                          DrawFrameControl(Canvas.Handle,ir,DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_PUSHED)
                        else
                          DrawFrameControl(Canvas.Handle,ir,DFC_BUTTON, DFCS_BUTTONPUSH);
                      end;

                      InflateRect(ir,-2,-2);
                      DrawText(Canvas.Handle,PChar(ControlValue),Length(ControlValue),ir,DT_CENTER or DT_VCENTER or DT_SINGLELINE);
                    end;
                  end;

                  if (ControlType = 'BUTTON') then
                    IMGSize.y := 25;

                  if (ControlType = 'COMBO') then
                     IMGSize.y := 25;

                  if (ControlType = 'CHECK') or (ControlType = 'RADIO') then
                  begin
                    IMGSize.x := Round(16 * ResFactor);
                    IMGSize.y := 16;
                  end;

                  if ((XPos - r.Left > w) and (XPos - r.Left < w + IMGSize.x) and
                     (YPos > cr.Top) and (YPos < cr.Top + IMGSize.Y)) or
                     (CheckHotSpot and (CID = ControlID)) then
                  begin
                    ImageHotSpot := True;
                    AnchorVal := 'ctrl';
                    AltImg := ImgIdx;

//                    ir := cr;
//                    ir.Right := cr.Left + IMGSize.x;

                    ir.Left := r.left + w;
                    ir.Right := ir.Left + ImgSize.X;
                    ir.Top := cR.Top;
                    ir.Bottom := cr.top + ImgSize.Y;

                    ControlRect := ir;
                    CV := ControlValue;
                    CID := ControlID;
                    CT := ControlType;
                  end;

                    if (w + IMGSize.x > r.Right-r.Left) and
                       (IMGSize.x < r.Right - r.Left) then
                    begin
                      ImgBreak := True;
                    end
                    else
                      begin
                        w := w + IMGSize.x;
                        cr.left := cr.left + IMGSize.x;
                        if IMGSize.y > h then
                          h := IMGSize.y;
                      end;

///                  cr.left := fr.left + Indent;
                end;
              end;
          'H':begin
                case Upcase(s[3]) of
                'R':
                  begin
                    LineBreak := True;
                    if not Calc then
                    begin
                      Pencolor := Canvas.Pen.color;
                      Canvas.Pen.color:=clblack;
                      Canvas.MoveTo(r.left,cr.bottom+1);
                      Canvas.Lineto(r.right,cr.bottom+1);
                      Canvas.pen.color:=pencolor;
                    end;
                  end;
                  'I':
                  begin
                    if not Calc then
                    begin
                      isHi := true;
                      hifCol := Canvas.Font.Color;
                      hibCol := Canvas.Brush.Color;
                      if Canvas.Brush.Style = bsClear then
                        hibCol := clNone;

                      Canvas.Brush.Color := HTMLHighLightColor;
                      Canvas.Font.Color := HTMLHighLightTextColor;
                    end;
                  end;
                end;
              end;
          'I':begin
                TagChar := Upcase(s[3]);

                if TagChar = '>' then // <I> tag
                  Canvas.Font.Style := Canvas.Font.Style + [fsItalic]
                else
                if TagChar = 'N' then  // <IND> tag
                begin
                  TagProp := Copy(s,3,pos('>',s)-1);

                  Prop := Copy(TagProp,ipos('x',TagProp)+2,Length(TagProp));
                  Prop := Copy(Prop,Pos(ASG_ATTR_DELIM,Prop)+1,Length(prop));
                  Prop := Copy(Prop,1,Pos(ASG_ATTR_DELIM,Prop)-1);

                  val(Prop,indent,err);
                  if err = 0 then
                  begin
                    if Indent > w then
                     begin
                       w := Indent;
                       cr.left := fr.left + Indent;
                     end;
                  end;
                end
                else
                  if TagChar = 'M' then
                  begin
                    inc(ImgIdx);

                    //oldfont.color:=Canvas.font.color;
                    TagProp := Uppercase(Copy(s,3,pos('>',s) - 1));
                    Prop := Copy(TagProp,Pos('SRC',TagProp) + 4,Length(TagProp));
                    Prop := Copy(Prop,Pos(ASG_ATTR_DELIM,Prop) + 1,Length(Prop));
                    Prop := Copy(Prop,1,Pos(ASG_ATTR_DELIM,Prop) - 1);

                    if (Pos('ALT',TagProp) > 0) and (AltImg = ImgIdx) then
                    begin
                      Prop := Copy(TagProp,Pos('ALT',TagProp) + 4,Length(TagProp));
                      Prop := Copy(Prop,Pos(ASG_ATTR_DELIM,Prop) + 1,Length(Prop));
                      Prop := Copy(Prop,1,Pos(ASG_ATTR_DELIM,Prop) - 1);
                    end;

                    TagWidth := 0;
                    TagHeight := 0;

                    if Pos('WIDTH',TagProp) > 0 then
                    begin
                      Tagp := Copy(TagProp,Pos('WIDTH',TagProp) + 6,Length(TagProp));
                      Tagp := Copy(Tagp,Pos(ASG_ATTR_DELIM,tagp) + 1,Length(Tagp));
                      Tagp := Copy(Tagp,1,Pos(ASG_ATTR_DELIM,tagp) - 1);
                      Val(Tagp,TagWidth,Err);
                    end;

                    if Pos('HEIGHT',TagProp) > 0 then
                    begin
                      Tagp := Copy(TagProp,ipos('HEIGHT',TagProp) + 7,Length(TagProp));
                      Tagp := Copy(Tagp,pos(ASG_ATTR_DELIM,Tagp) + 1,Length(Tagp));
                      Tagp := Copy(Tagp,1,pos(ASG_ATTR_DELIM,Tagp) - 1);
                      Val(Tagp,TagHeight,Err);
                    end;

                    IMGSize.x := 0;
                    IMGSize.y := 0;

                    if Pos('IDX:',Prop) > 0 then
                    begin
                      Delete(Prop,1,4);
                      if Assigned(FImages) and (IStrToInt(Prop) < FImages.Count) then
                      begin
                        IMGSize.x := MulDiv(FImages.Width,GetDeviceCaps(Canvas.Handle,LOGPIXELSX),96);
                        IMGSize.y := MulDiv(FImages.Height,GetDeviceCaps(Canvas.Handle,LOGPIXELSY),96);

                        if not Calc and not Print then
                          FImages.Draw(Canvas,cr.Left,cr.Top,IStrToInt(Prop),True);

                        if not Calc and Print then
                        begin
                          cr.Right := cr.Left + Round(ResFactor * FImages.Width);
                          cr.Bottom := cr.Top + Round(ResFactor * FImages.Height);

                          ABitmap := TBitmap.Create;
                          FImages.GetBitmap(IStrToInt(Prop),ABitmap);
                          PrintBitmap(Canvas,cr,ABitmap);
                          ABitmap.Free;
// {changed}               cr := r;
                        end;
                      end;
                    end;

                    if Pos('SSYS:',Prop) > 0 then
                    begin
                      Delete(Prop,1,5);
                      IMGSize := SysImage(Canvas,cr.Left,cr.Top,Prop,False,not Calc,Print,ResFactor);

                      IMGSize.x := MulDiv(IMGSize.X,GetDeviceCaps(Canvas.Handle,LOGPIXELSX),96);
                      IMGSize.y := MulDiv(IMGSize.Y,GetDeviceCaps(Canvas.Handle,LOGPIXELSY),96);
                    end;

                    if Pos('LSYS:',Prop) > 0 then
                    begin
                      Delete(Prop,1,5);
                      IMGsize := SysImage(Canvas,cr.Left,cr.Top,Prop,True,not Calc,Print,ResFactor);

                      IMGSize.x := MulDiv(IMGSize.X,GetDeviceCaps(Canvas.Handle,LOGPIXELSX),96);
                      IMGSize.y := MulDiv(IMGSize.Y,GetDeviceCaps(Canvas.Handle,LOGPIXELSY),96);
                    end;

                    bmp := nil;

                    if (Pos(':',Prop) = 0) and Assigned(pc) then
                    begin
                      bmp := pc.FindPicture(Prop);
                    end;

                    if (Pos('://',Prop) > 0) and Assigned(ic) then
                    begin
                      if ic.FindPicture(Prop) = nil then
                        with ic.AddPicture do
                        begin
                          Asynch := False;
                          LoadFromURL(Prop);
                        end;

                      bmp := ic.FindPicture(Prop) as THTMLPicture;
                    end;

                      if bmp <> nil then
                      begin
                        if not bmp.Empty then
                        begin
                          if (TagWidth > 0) and (TagHeight > 0) then
                          begin
                            imgw := TagWidth;
                            imgh := TagHeight;
                          end;
                          if (TagWidth > 0) and (TagHeight = 0) then
                          begin
                            imgw := TagWidth;
                            imgh := Round(TagWidth/bmp.Width * bmp.Height);
                          end;
                          if (TagWidth = 0) and (TagHeight > 0) then
                          begin
                            imgw := Round(TagHeight/bmp.Height * bmp.Width);
                            imgh := TagHeight;
                          end;
                          if (TagWidth = 0) and (TagHeight = 0) then
                          begin
                            imgw := bmp.Width;
                            imgh := bmp.Height;
                          end;
                          if not Calc {and not Print} then
                          begin
                            if (TagWidth > 0) and (TagHeight > 0) then
                            begin
                              imgw := TagWidth;
                              imgh := TagHeight;
                            end;
                            if (TagWidth > 0) and (TagHeight = 0) then
                            begin
                              imgw := TagWidth;
                              imgh := Round(TagWidth/bmp.Width * bmp.Height);
                            end;
                            if (TagWidth = 0) and (TagHeight > 0) then
                            begin
                              imgw := Round(TagHeight/bmp.Height * bmp.Width);
                              imgh := TagHeight;
                            end;
                            if (TagWidth = 0) and (TagHeight = 0) then
                            begin
                              imgw := bmp.Width;
                              imgh := bmp.Height;
                            end;

                            if (TagWidth > 0) or (TagHeight > 0) then
                            begin
                              if (bmp is THTMLPicture) then
                                (bmp as THTMLPicture).Stretch := True;
                              Canvas.StretchDraw(Rect(cr.Left,cr.Top,cr.Left + imgw,cr.Top + imgh),bmp)
                            end
                            else
                            begin
                              // need for animation - redraw background
                              if (bmp is THTMLPicture) then
                              begin

                                if (bmp as THTMLPicture).FrameCount > 1 then
                                begin
                                  Canvas.Pen.Color := BlnkColor;
                                  Canvas.Brush.Color := BlnkColor;
                                  Canvas.Rectangle(cr.Left,cr.Top,cr.Left + (bmp as THTMLPicture).MaxWidth,cr.Top + (bmp as THTMLPicture).MaxHeight);
                                end;

                                Canvas.Draw(cr.Left + (bmp as THTMLPicture).FrameXPos,cr.Top + (bmp as THTMLPicture).FrameYPos,bmp);
                              end
                              else
                                Canvas.Draw(cr.Left,cr.Top,bmp);
                            end;
                          end;

                          if (TagWidth > 0) or (TagHeight > 0) then
                          begin
                            IMGSize.x := MulDiv(imgw,GetDeviceCaps(Canvas.Handle,LOGPIXELSX),96);
                            IMGSize.y := MulDiv(imgh,GetDeviceCaps(Canvas.Handle,LOGPIXELSY),96);
                          end
                          else
                          begin
                            if (bmp is THTMLPicture) then
                            begin
                              IMGSize.x := MulDiv((bmp as THTMLPicture).MaxWidth,GetDeviceCaps(Canvas.Handle,LOGPIXELSX),96);
                              IMGSize.y := MulDiv((bmp as THTMLPicture).MaxHeight,GetDeviceCaps(Canvas.Handle,LOGPIXELSY),96);
                            end
                            else
                            begin
                              IMGSize.x := MulDiv(bmp.Width,GetDeviceCaps(Canvas.Handle,LOGPIXELSX),96);
                              IMGSize.y := MulDiv(bmp.Height,GetDeviceCaps(Canvas.Handle,LOGPIXELSY),96);
                            end;
                          end;
                        end;
                      end;

                    if (XPos - r.Left > w) and (XPos - r.Left < w + IMGSize.x) and
                       (YPos > cr.Top) and (YPos < cr.Top + IMGSize.Y) and Anchor then
                    begin
                      ImageHotSpot := True;
                      AnchorVal := LastAnchor;
                      AnchorHint := LastAnchorHint;
                      AltImg := ImgIdx;
                    end;

                    if Print then
                    begin
                      //IMGSize.x := Round(IMGSize.x * ResFactor);
                      //IMGSize.y := Round(IMGSize.y * ResFactor);
                      {$IFDEF TMSDEBUG}
                      DbgPoint('bmp : ',point(IMGSize.x,IMGSize.y));
                      {$ENDIF}
                    end;

                    if (w + IMGSize.x > r.Right-r.Left) and
                       (IMGSize.x < r.Right - r.Left) then
                    begin
                      ImgBreak := True;
                    end
                    else
                      begin
                        w := w + IMGSize.x;
                        cr.left := cr.left + IMGSize.x;
                        if IMGSize.y > h then
                          h := IMGSize.y;
                      end;

                    if Pos('ALIGN',TagProp) > 0 then
                    begin
                      if Pos('"TOP',TagProp) > 0 then
                      begin
                        ImgAli := h - Canvas.TextHeight('gh');
                      end
                      else
                      begin
                        if Pos('"MIDDLE',TagProp) > 0 then
                          ImgAli := (h - Canvas.TextHeight('gh')) shr 1;
                      end;
                    end;
                  end;
                end;
          'L':begin
                w := w + 12 * ListIndex;
                if Linkbreak then
                  Imgbreak := True
                else
                  Linkbreak := True;

                cr.left := cr.left + 12 * (ListIndex - 1);

                if not calc and not Invisible then
                begin
                  Prop := Canvas.Font.Name;
                  Canvas.Font.Name := 'Symbol';

                  if not OList then
                  begin
                    if Odd(ListIndex) then
                      DrawText(Canvas.Handle,'·',1,cr,0)
                    else
                      DrawText(Canvas.Handle,'o',1,cr,0);
                  end
                  else
                  begin
                    OListIndexStr := IntToStr(OListIndex)+'. ';
                    Inc(OListIndex);

                    DrawText(Canvas.Handle,PChar(OListIndexStr),Length(OListIndexStr),cr,0);
                    cr.Left := cr.Left + Canvas.TextWidth(OListIndexStr) - 12;
                  end;

                  Canvas.Font.Name:=prop;
                end;
                cr.Left := cr.Left + 12;
              end;
          'U':begin
                if s[3] <> '>' then
                begin
                  if not Calc then
                  begin
                    Inc(ListIndex);
                    OList := false;
                  end;
                  if w > 0 then
                    LineBreak := True;
                end
                else
                  Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];
              end;
          'O':begin
                TagChar := Upcase(s[3]);
                if TagChar = 'L' then  // <OFS> tag
                begin
                  if not Calc then
                    Inc(ListIndex);
                  if w > 0 then
                    LineBreak := true;
                  OList := true;
                  OListIndex := 1;
                end;
              end;

          'P':begin
                //if calc then
                //  Linebreak := true
                //else
                begin
                  if (VarPos('>',s,TagPos)>0) then
                  begin
                    TagProp := Uppercase(Copy(s,3,TagPos-1));

                    if VarPos('ALIGN',TagProp,TagPos) > 0 then
                    begin
                      Prop := Copy(TagProp,TagPos+5,Length(TagProp));
                      Prop := Copy(Prop,Pos(ASG_ATTR_DELIM,prop)+1,Length(Prop));
                      Prop := Copy(Prop,1,Pos(ASG_ATTR_DELIM,prop)-1);

                      if Pos('RIGHT',Prop) > 0 then Align := taRightJustify;
                      if Pos('LEFT',Prop) > 0 then Align := taLeftJustify;
                      if Pos('CENTER',Prop) > 0 then Align := taCenter;
                    end;

                    if VarPos('INDENT',TagProp,TagPos) > 0 then
                    begin
                      Prop := Copy(TagProp,TagPos+6,Length(TagProp));
                      Prop := Copy(Prop,Pos(ASG_ATTR_DELIM,prop)+1,Length(Prop));
                      Prop := Copy(Prop,1,Pos(ASG_ATTR_DELIM,prop)-1);
                      PIndent := IStrToInt(Prop);
                    end;


                    if VarPos('BGCOLOR',TagProp,TagPos) > 0 then
                    begin
                      Prop := Copy(TagProp,TagPos + 5,Length(TagProp));
                      Prop := Copy(Prop,Pos(ASG_ATTR_DELIM,Prop) + 1,Length(Prop));
                      Prop := Copy(Prop,1,Pos(ASG_ATTR_DELIM,Prop) - 1);

                      NewColor := clNone;

                      if Length(Prop) > 0 then
                      begin
                        if Prop[1] = '#' then
                          NewColor := Hex2Color(Prop)
                        else
                          NewColor := Text2Color(AnsiLowerCase(prop));
                      end;

                      if not Calc then
                      begin
                        isPara := True;
                        paracolor := Canvas.Brush.Color;
                        if Canvas.Brush.Style = bsClear then ParaColor := clNone;
                        Canvas.Brush.color := NewColor;
                        PenColor:=Canvas.Pen.Color;
                        Canvas.Pen.Color := Newcolor;
                        Canvas.Rectangle(fr.left,r.top,fr.right,r.bottom);
                      end;
                    end;
                  end;
                end;
            end;
        'F':begin
              if (VarPos('>',s,TagPos)>0) then
              begin
                TagProp := UpperCase(Copy(s,6,TagPos-6));

                if (VarPos('FACE',TagProp,TagPos) > 0) then
                begin
                  Prop := Copy(TagProp,TagPos+4,Length(TagProp));
                  Prop := Copy(prop,pos(ASG_ATTR_DELIM,prop)+1,Length(prop));
                  Prop := Copy(prop,1,pos(ASG_ATTR_DELIM,prop)-1);
                  Canvas.Font.Name := Prop;
                end;

                if (VarPos(' COLOR',TagProp,TagPos) > 0) and not Selected then
                begin
                  Prop := Copy(TagProp,TagPos+6,Length(TagProp));
                  Prop := Copy(Prop,Pos(ASG_ATTR_DELIM,prop)+1,Length(prop));
                  Prop := Copy(Prop,1,Pos(ASG_ATTR_DELIM,prop)-1);
                  //oldfont.color:=Canvas.font.color;

                  if Length(Prop) > 0 then
                  begin
                    if Prop[1] = '#' then
                      Canvas.font.color := Hex2Color(Prop)
                    else
                      Canvas.Font.Color := Text2Color(AnsiLowerCase(prop));
                  end;

                end;

                if (VarPos('BGCOLOR',TagProp,TagPos)>0) and not Calc and not Selected then
                begin
                  Prop := Copy(TagProp,TagPos+7,Length(TagProp));
                  Prop := Copy(prop,pos(ASG_ATTR_DELIM,prop)+1,Length(prop));
                  Prop := Copy(prop,1,pos(ASG_ATTR_DELIM,prop)-1);
                  BGColor := Canvas.Brush.Color;

                  if Canvas.Brush.Style = bsClear then
                    bgcolor := clNone;

                  if Length(Prop) > 0 then
                  begin
                    if Prop[1] = '#' then
                      Canvas.Brush.Color := Hex2Color(Prop)
                    else
                      Canvas.Brush.Color := Text2Color(AnsiLowerCase(prop));
                  end;
                end;

                if (VarPos('STYLE',TagProp,TagPos)>0) then
                begin
                  Prop := Copy(TagProp,TagPos+5,Length(TagProp));
                  Prop := Copy(Prop,Pos('=',Prop)+1,Length(Prop));
                  Prop := Copy(Prop,Pos(ASG_ATTR_DELIM,Prop)+1,Length(Prop));
                  // font-size:14pt
                  if Pos('FONT-SIZE:', Uppercase(Prop)) > 0 then
                  begin
                    Delete(Prop, 1, 10);

                    if pos('PT', UpperCase(Prop)) > 0 then
                    begin
                      Prop := Trim(Copy(Prop,1, Pos('PT',Uppercase(Prop))-1));
                      Canvas.Font.Size := IStrToInt(Prop);
                    end;
                  end;
                end
                else
                if (VarPos('SIZE',TagProp,TagPos)>0) then
                begin
                  Prop := Copy(TagProp,TagPos+4,Length(TagProp));
                  Prop := Copy(Prop,Pos('=',Prop)+1,Length(Prop));
                  Prop := Copy(Prop,Pos(ASG_ATTR_DELIM,Prop)+1,Length(Prop));

                  case IStrToInt(Prop) of
                  1:Canvas.Font.Size := 8;
                  2:Canvas.Font.Size := 10;
                  3:Canvas.Font.Size := 12;
                  4:Canvas.Font.Size := 14;
                  5:Canvas.Font.Size := 16;
                  else
                    Canvas.Font.Size := IStrToInt(Prop);
                  end;
                end;
              end;
            end;
        'S':begin
              TagChar := Upcase(s[3]);

              if TagChar = '>' then
                Canvas.Font.Style := Canvas.font.Style + [fsStrikeOut]
              else
              begin
                if TagChar = 'T' then
                begin
                  Canvas.Font.Style := Canvas.font.Style + [fsStrikeOut]
                end
                else
                if TagChar = 'P' then
                begin
                  if (VarPos('>',s,TagPos)>0) then
                  begin

                    TagProp := UpperCase(Copy(s,6,TagPos-6));
                    if (VarPos('STYLE',TagProp,TagPos)>0) then
                    begin
                      Prop := Copy(TagProp,TagPos+5,Length(TagProp));
                      Prop := Copy(Prop,Pos('=',Prop)+1,Length(Prop));
                      Prop := Copy(Prop,Pos(ASG_ATTR_DELIM,Prop)+1,Length(Prop));
                      // font-size:14pt
                      if Pos('BACKGROUND-COLOR:', Uppercase(Prop)) > 0 then
                      begin
                        Delete(Prop, 1, 17);

                        if pos('#', UpperCase(Prop)) = 1 then
                        begin
                          BGColor := Canvas.Brush.Color;
                          Canvas.Brush.Color := Hex2Color(Prop);
                        end;
                      end;
                    end;
                  end;
                end
                else
                if TagChar = 'H' then
                  isShad := True
                else
                begin
                  if ipos('<SUB>',s)=1 then
                    isSub := True
                  else
                    if ipos('<SUP>',s)=1 then
                      isSup := True;
                end;
              end;
            end;
        'R':begin
              TagProp := Copy(s,3,pos('>',s)-1);
              prop := Copy(TagProp,ipos('a',TagProp)+2,Length(TagProp));
              prop := Copy(prop,pos(ASG_ATTR_DELIM,prop)+1,Length(prop));
              prop := Copy(prop,1,pos(ASG_ATTR_DELIM,prop)-1);
              Val(prop,Indent,err);
              StartRotated(Canvas,indent);
            end;
        'Z':Invisible := True;
        end;
      end;

      if (VarPos('>',s,TagPos) > 0) and not ImgBreak then
      begin
        Res := Res + Copy(s,1,TagPos);
        Delete(s,1,TagPos);
      end
      else
      begin
        if not Imgbreak then
          Delete(s,1,Length(s));
      end;
    end;

  end;

    w := w - sw;

    if w > xsize then
      xsize := w;

    if (FocusLink = Hyperlinks-1) and Anchor and not Calc then
    begin
      rr.Right := cr.Left;
      rr.Bottom := cr.Bottom;
      InflateRect(rr,1,0);
      if not Calc then
        Canvas.DrawFocusRect(rr);
      rr.Left := r.Left + 1;
      rr.Top := rr.Bottom;
    end;

    Result := Res;
  end;

  function DrawTextBidimodeFlags(DrawStyle: integer; BidiMode: TBidiMode): integer;
  var
    bool : boolean;

  begin
    bool := SysLocale.MiddleEast and (BiDiMode = bdRightToLeft);
    Result := DrawStyle;
    // do not change center alignment 
    if bool then
      if Result and DT_RIGHT = DT_RIGHT then
        Result := Result and not DT_RIGHT { removing DT_RIGHT, makes it DT_LEFT }
      else 
      if not (Result and DT_CENTER = DT_CENTER) then
        Result := Result or DT_RIGHT;

    bool := SysLocale.MiddleEast and (BiDiMode <> bdLeftToRight);

    if bool then 
      Result := Result or DT_RTLREADING;
  end;

begin
  Anchor := False;
  Error := False;
  OrgPenColor := Canvas.Pen.Color;
  OrgPenWidth := Canvas.Pen.Width;
  OldFont := TFont.Create;
  OldFont.Assign(Canvas.Font);
  DrawFont := TFont.Create;
  DrawFont.Assign(Canvas.Font);
  CalcFont := TFont.Create;
  CalcFont.Assign(Canvas.Font);
  OldDrawfont := TFont.Create;
  OldDrawFont.Assign(Canvas.Font);
  OldCalcFont := TFont.Create;
  OldCalcFont.Assign(Canvas.Font);
  BlnkColor := Canvas.Brush.color;
  Canvas.Brush.Color := clNone;
  BGColor := clNone;
  ParaColor := clNone;
  isPara := False;
  isShad := False;
  Invisible := False;

  if IsWinXP then
    UseWinXP := IsThemeActive
  else
    UseWinXP := False;

  // Control param initialization
  ControlRect := Rect(0,0,0,0);
  CV := '';
  CT := '';
  if not CheckHotSpot then
    CID := '';

  Result := False;

  r := fr;
  r.Left := r.Left + 1; {required to add offset for DrawText problem with first capital W letter}

  Align := taLeftJustify;
  PIndent := 0;

  XSize := 0;
  YSize := 0;
  HyperLinks := 0;
  HlCount := 0;
  ListIndex := 0;
  LiCount := 0;
  StripVal := '';
  FocusAnchor := '';
  AnchorHint := '';
  MouseLink := -1;
  MouseInAnchor := False;

  if ResFactor < 1 then
    ResFactor := 1;

  ImgIdx := 0;
  AltImg := -1;

  SetBKMode(Canvas.Handle,TRANSPARENT);

  DrawStyle := DT_LEFT or DT_SINGLELINE or DT_EXTERNALLEADING or DT_BOTTOM  or DT_EXPANDTABS {or DT_NOPREFIX};

  DrawStyle := DrawTextBidimodeFlags(DrawStyle,bidiMode);

  if not WordWrap then
    DrawStyle := DrawStyle or DT_END_ELLIPSIS;

  if Pos('&',s) > 0 then
  begin
    repeat
      Foundtag := False;
      //if TagReplacestring('&lt;','<',s) then Foundtag := True;
      //if TagReplacestring('&gt;','>',s) then Foundtag := True;

      if TagReplacestring('&amp;','&&',s) then Foundtag := True;
      if TagReplacestring('&quot;','"',s) then Foundtag := True;

      if TagReplacestring('&sect;','§',s) then Foundtag := True;
      if TagReplacestring('&permil;','‰',s) then Foundtag := True;
      if TagReplacestring('&reg;','®',s) then Foundtag := True;

      if TagReplacestring('&copy;','©',s) then Foundtag := True;
      if TagReplacestring('&para;','¶',s) then Foundtag := True;

      if TagReplacestring('&trade;','™',s) then Foundtag := True;
      if TagReplacestring('&euro;','€',s) then Foundtag := True;

    until not Foundtag;
  end;

  s := DBTagStrip(s);
  s := CRLFStrip(s,ASG_KEEP_LINEBREAKS);

  InsPoint := 0;

  while Length(s) > 0 do
  begin
    // calculate part of the HTML text fitting on the next line
    Oldfont.Assign(OldCalcFont);
    Canvas.Font.Assign(CalcFont);
    Oldanchor := Anchor;
    OldAnchorVal := LastAnchor;
    OldAnchorHint := LastAnchorHint;
    suph := 0;
    subh := 0;
    imgali := 0;
    isSup := False;
    isSub := False;
    isHi := False;

    HtmlHeight := Canvas.TextHeight('gh');
    txtHeight := HTMLHeight;

    OldImgIdx := ImgIdx;

    su := HTMLDrawLine(Canvas,s,r,True,HtmlWidth,HtmlHeight,subh,suph,imgali,Align,PIndent,XPos,YPos,HotSpot,ImageHotSpot);

    Anchor := OldAnchor;
    LastAnchor := OldAnchorVal;
    LastAnchorHint := OldAnchorHint;

    CalcFont.Assign(Canvas.Font);
    OldCalcFont.Assign(OldFont);

    dr := r;

    case Align of
    taCenter:if (r.right - r.left - htmlwidth > 0) then
               dr.left := r.left+((r.right - r.left - htmlwidth) shr 1);
    taRightJustify:if r.right - htmlwidth > r.left then
                       dr.left := r.right - htmlwidth;
    end;

    dr.Left := dr.Left + PIndent;
    dr.Bottom := dr.Top + HtmlHeight + Subh + Suph;

    if not CheckHeight then
    begin
      OldFont.Assign(OldDrawFont);
      Canvas.Font.Assign(DrawFont);

      HyperLinks := HlCount;
      ListIndex := LiCount;
      ImgIdx := OldImgIdx;

      HTMLDrawLine(Canvas,su,dr,CheckHotSpot,HtmlWidth,HtmlHeight,subh,suph,ImgAli,Align,PIndent,XPos,YPos,HotSpot,ImageHotspot);

      HlCount := HyperLinks;
      LiCount := ListIndex;

      if (HotSpot and
         (YPos > dr.Bottom - ImgAli - Canvas.TextHeight('gh')) and
         (YPos < dr.Bottom - ImgAli)) or ImageHotSpot then
        Result := True;

      DrawFont.Assign(Canvas.Font);
      OldDrawFont.Assign(OldFont);
    end;

    r.top := r.top + HtmlHeight + subh + suph;
    ysize := ysize + HtmlHeight + subh + suph;

    // do not draw below bottom
    if (r.top + txtheight > r.bottom) and not CheckHeight then
      s := '';
  end;

  if (ysize = 0) then
    ysize := Canvas.TextHeight('gh');

  InsPoint := InsPoint shr 1;

  Canvas.Pen.Color := OrgPenColor;
  Canvas.Pen.Width := OrgPenWidth;

  Canvas.Brush.Color := BlnkColor;
  Canvas.Font.Assign(OldFont);
  OldFont.Free;
  DrawFont.Free;
  CalcFont.Free;
  OldDrawfont.Free;
  OldCalcfont.Free;
end;
{$WARNINGS ON}


{$IFNDEF REMOVEDRAW}
function HTMLDraw(Canvas: TCanvas;s:string;fr:trect;
                                 FImages:TCustomImageList;
                                 xpos,ypos:integer;
                                 checkhotspot,checkheight,print,selected,blink:boolean;
                                 resfactor:double;
                                 URLColor:tcolor;
                                 var Anchorval,StripVal:string;
                                 var XSize,YSize:integer;
                                 Bidimode: TBidiMode = bfLeftToRight):boolean;
var
  HyperLinks,MouseLink: Integer;
  Focusanchor,ControlID,ControlValue,ControlType: string;
  r: TRect;
  cr: TRect;
begin
  Result := HTMLDrawEx(Canvas,s,fr,FImages,xpos,ypos,-1,-1,1,checkhotspot,checkheight,print,selected,blink,false,
                       False,false,resfactor,URLColor,clNone,clNone,clGray,anchorval,stripval,focusanchor,xsize,ysize,
                       HyperLinks,MouseLink,r,cr,ControlID,ControlValue,ControlType,nil,nil,0,BidiMode);
end;

{$IFNDEF REMOVEIPOSFROM}
function IPosFrom(su,s:string;frm:integer):Integer;
var
  i:Integer;
begin
  i := Pos(UpperCase(su),UpperCase(s));
  if i > frm then
    Result := i
  else
    Result := 0;
end;
{$ENDIF}

{$ENDIF}


{$IFNDEF REMOVESTRIP}

function HTMLStrip(s:string):string;
var
  Tpf, Tpn: integer;
begin
  Result := '';
  // replace line breaks by linefeeds
  while (pos('<BR>',uppercase(s))>0) do s := StringReplace(s,'<BR>',chr(13)+chr(10),[rfIgnoreCase]);
  while (pos('<HR>',uppercase(s))>0) do s := StringReplace(s,'<HR>',chr(13)+chr(10),[rfIgnoreCase]);

  {remove all other tags}
  while (VarPos('<',s,Tpf) > 0) do
  begin
    Result := Result + Copy(s, 1, Tpf - 1);
    if (VarPos('>',s, Tpn)>0) then
      Delete(s,1,Tpn)
    else
    begin
      Delete(s,1,Tpf - 1);
      Break;
    end;
  end;
  Result := Result + s;
end;
{$ENDIF}

{$IFDEF HILIGHT}

function HTMLStripAll(s:string):string;
var
  TagPos, PTP: integer;
begin
  Result := '';

  // remove all tags
  while (VarPos('<',s,TagPos)>0) do
  begin
    PTP := TagPos;
    if (VarPos('>',s,TagPos) > PTP) then
    begin
      Result := Result + Copy(s,1,PTP - 1);
      Delete(s,1,TagPos)
    end
    else
      Break;
  end;
  Result := Result + s;
end;

function StripPos2HTMLPos(s:string; i: Integer): Integer;
var
  j,k: Integer;
  Skip: Boolean;
begin
  Result := 0;
  k := 1;
  Skip := False;

  for j := 1 to Length(s) do
  begin
    if s[j] = '<' then
      Skip := True;

    if k = i then
    begin
      Result := j;
      Exit;
    end;

    if not Skip then
      Inc(k);

    if s[j] = '>' then
      Skip := False;

  end;

  if k = i then
  begin
    Result := Length(s) + 1;
  end;
end;


function PosFrom(su,s:string; h: Integer;DoCase: boolean; var Res: Integer): Integer;
var
  r: Integer;
begin
  Result := 0;
  Res := 0;

  if h > 0 then
    Delete(s,1,h);

  if DoCase then
    r := Pos(su,s)
  else
    r := Pos(AnsiUpperCase(su),AnsiUpperCase(s));

  if r > 0 then
  begin
    Res := h + r;
    Result := Res;
  end;
end;

function HiLight(s,h,tag:string;DoCase,FullText:boolean):string;
var
  hs: string;
  l,k,m: Integer;
  IsHTML: Boolean;
begin
  // This code removes other highlighting, i.e. all "<"+Tag+">" and "</"+Tag+">"
  // - this will avoid a "duplicate" highlighting as this could lead to highlight
  //   unappropriated parts of a text or confuse a user
  // To avoid case sensitivity I use the fast ASCII version of Uppercase function
  // as this is good enough for all tags (they can’t contain international characters)
  tag := Uppercase(tag); // an upper-case version of a tag used in the rest of the function
  hs := Uppercase(s); // an upper-case version of the searched text
  // I repeatedly look for "<"+tag+">" and remove it from both upper-case (hs) and original (s)
  // version of the searched text. This part can be better optimized, but in most cases
  // the highlight tag won’t be presented in the searched text or it will be there only once.

  l := Pos('<' + tag + '>',hs);
  while (l > 0) do
  begin
    Delete(s, l, Length(tag) + 2);
    Delete(hs, l, Length(tag) + 2);
    l := Pos('<' + tag + '>',hs);
  end;

  // Same for "</"+tag+">"
  l := Pos('</' + tag + '>',hs);
  while (l > 0) do
  begin
    Delete(s, l, Length(tag) + 3);
    Delete(hs, l, Length(tag) + 3);
    l := Pos('</' + tag + '>',hs);
  end;

  // This code is little confusing as it is not always clear when the text is HTML and when
  // is just a plain text. Some parts of TAdvStringGrid code recognizes HTML text by a presence
  // of "</" string in the text and others handle all texts like HTML. I have selected the first
  // version (the presence of "</") as I hope it is the wanted behaviour.

  // Convert h to upper-case version for non-case-sensitive search.
  // The ANSI version has been used to convert all letters including international ones.
  if not DoCase then
    h := AnsiUppercase(h);
  // Now I try to detect presence of HTML in the searched text
  IsHTML := (Pos('</', s) > 0);
  // For HTML version I need to create a plain text version without all tags and special characters
  // converted to the normal ones.
  if IsHTML then
    hs := UnFixMarkup(HTMLStripAll(s), true)

  else hs := s;
  // For non-case-sensitive search I need the upper-case version.
  if not DoCase then
    hs := AnsiUppercase(hs);

  // I will clear the result as I need to add handled parts to it, to avoid an extra variable.
  Result := '';

  // When the full text search is provided (i.e. both texts must match) I need just to compare
  // h and hs. If they match I can highlight the whole text.
  if FullText then
  begin
    // This works for both html and plain texts, but for HTML version the version with tags
    // should be included in the result opposite to plain text version where we need the version
    // with fixed special characters
    if (h = hs) then
      if (IsHTML) then s := '<' + tag + '>' + s + '</' + tag + '>'
      else s := '<' + tag + '>' + FixMarkup(s) + '</' + tag + '>';
  end
  else
  if not IsHTML then
  begin
    // The plain-text partial search is little more complicated. Mainly I need to convert all special
    // characters to the HTML version when the highlighting is applied. Otherwise I need to keep the
    // original plain-text version as there is not tag in the text and it will be handled as plain text.
    l := Pos(h, hs);
    if (l > 0) then
    begin
      repeat
        Result := Result + FixMarkup(Copy(s, 1, l - 1), false) + '<' + tag + '>' + FixMarkup(Copy(s, l, Length(h)), false) + '</' + tag + '>';
        Delete(s, 1, l + length(h) - 1);
        Delete(hs, 1, l + length(h) - 1);
        l := Pos(h, hs);
      until (l = 0);
      s := FixMarkup(s, false);
    end;
  end
  else
  begin
    // The last and more complex processing for HTML texts. The main idea is to find the text fh (i.e.
    // h with fixed special characters) in the searched text hs (i.e. s without tags). When I found
    // a matching, I will copy everything from the original text up to the matching location including
    // tags.
    // Try to find a matching and store it to L.
    l := Pos(h, hs);
    while (l > 0) do
    begin
      // Some matching was found, I can safely delete everything up to the end of matching
      // from hs (s without tags)
      Delete(hs, 1, l + length(h) - 1);
      // Now I need to move a part of s before the matching to the Result.
      // But it still can contain both tags and special characters!
      repeat
        // Try to find a start of the first tag or special character in remaining part of s, if it exists.
        k := pos('<', s);
        m := pos('&', s);
        // Move all text, all tags up to the matching and all special characters before the matching
        // to the result. Keep on mind the tags are not calculated, but special characters are handled as
        // a single character.
        while ((k > 0) and (k <= l)) or ((m > 0) and (m < l)) do
        begin
          // We need to move the part of S before the matching and the tag/special character into the result,
          // whatever of them is the first.
          if (k > 0) and ((m <= 0) or (k < m)) then
          begin
            Result := Result + copy(s, 1, k - 1);
            Delete(s, 1, k - 1);
            l := l - (k - 1);
            m := m - (k - 1);
            k := pos('>', s);
            if (k = 0) then k := Length(s);
            Result := Result + copy(s, 1, k);
            Delete(s, 1, k);
            m := m - k;
            k := pos('<', s);
          end
          else
          begin
            Result := Result + copy(s, 1, m - 1);
            Delete(s, 1, m - 1);
            l := l - (m - 1) - 1; // -1 = special character
            k := k - (m - 1);
            m := pos(';', s);
            if (m = 0) then m := Length(s);
            Result := Result + copy(s, 1, m);
            Delete(s, 1, m);
            k := k - m;
            m := pos('&', s);
          end;
        end;
        // No more tags or special characters were found or they lie after a start of the matching or the
        // special character is the first character of the matching. I can move a part of S before
        // the matching to Result and start highlighting adding "<"+tag+">"
        Result := Result + copy(s, 1, l - 1) + '<' + tag + '>';
        Delete(s, 1, l - 1);
        // Now I will exclude the deleted part of s from the position of the first tag/spec.char
        // to re-use the values.
        k := k - (l - 1);
        m := m - (l - 1);
        // Store the length of matching text. Again, it can contain other tags.
        l := Length(h);
        // While there exists some tag in the matching text, move part of S before the tag into the result,
        // move the tag and try to find another one, until no one is found, it lies behind the matching text
        // or the end of S is reached (the last condition should never occur)
        while (((k > 0) and (k <= l)) or ((m > 0) and (m <= l))) and (l > 0) and (s <> '') do
        begin
          if (k > 0) and ((m <= 0) or (k < m)) then
          begin
            Result := Result + copy(s, 1, k - 1);
            Delete(s, 1, k - 1);
            l := l - (k - 1);
            m := m - (k - 1);
            k := pos('>', s);
            if (k = 0) then k := Length(s);
            Result := Result + copy(s, 1, k);
            // A special handling to keep highlighting after </font> tag
            if (Uppercase(copy(s, 1, 6)) = '</FONT') then
              Result := Result + '</' + tag + '><' + tag + '>';
            Delete(s, 1, k);
            m := m - k;
            k := pos('<', s);
          end
          else
          begin
            Result := Result + copy(s, 1, m - 1);
            Delete(s, 1, m - 1);
            l := l - (m - 1) - 1; // -1 = special character
            k := k - (m - 1);
            m := pos(';', s);
            if (m = 0) then m := Length(s);
            Result := Result + copy(s, 1, m);
            Delete(s, 1, m);
            k := k - m;
            m := pos('&', s);
          end;
        end;
        // L contains resting characters to move from S to the result. We don't need any other testing now.
        if (l > 0) then
        begin
          Result := Result + copy(s, 1, l);
          Delete(s, 1, l);
        end;
        // The whole matching was moved to result, so we must "close" highlighting now.
        Result := Result + '</' + tag + '>';
        l := 0;
        // Continue until whole string before the matching and the matching are moved
        // from S into the result.
      until (l = 0);
      // Now try to find another matching.
      l := Pos(h, hs);
    end;
  end;
  // S contains an unhandled part of the original text without any other matching.
  // - for full-text and plain-text matching the variable S can contain pre-processed
  //   values as the handling was different
  Result := Result + s;

  (*
  hs := HTMLStripAll(s);

  if FullText then
  begin
    if ((h = hs) and DoCase) or (not DoCase and (Uppercase(h) = Uppercase(hs))) then
      s := '<' + tag + '>' + s + '</' + Tag + '>';
  end
  else
  begin
    l := 0;
    if (pos(h,s) > 0) and ((pos('<',hs) >0) or (pos('>',hs) > 0)) then
    begin
      hs := StringReplace(hs,'<','&lt;',[rfReplaceAll]);
      hs := StringReplace(hs,'>','&gt;',[rfReplaceAll]);
      s := hs;
      h := StringReplace(h,'<','&lt;',[rfReplaceAll]);
      h := StringReplace(h,'>','&gt;',[rfReplaceAll]);
    end;

    while PosFrom(h,hs,l,DoCase,k) > 0 do
    begin
      l := k + Length(h);
      Insert('<'+tag+'>',s,StripPos2HTMLPos(s,k));
      Insert('</'+tag+'>',s,StripPos2HTMLPos(s,l));
    end;

  end;
  Result := s;
  *)
end;

function UnHiLight(s,tag:string):string;
var
  IsHTML, WasHTML: Boolean;
begin
  Result := '';
  // replace line breaks by linefeeds
  WasHTML := (Pos('</', s) > 0);

  tag := Uppercase(tag);
  while Pos('<' + tag + '>', Uppercase(s)) > 0 do s := StringReplace(s,'<' + tag + '>','',[rfIgnoreCase]);
  while Pos('</' + tag + '>', Uppercase(s)) > 0 do s := StringReplace(s,'</' + tag + '>','',[rfIgnoreCase]);

  IsHTML := (Pos('</', s) > 0);
  if WasHTML and not IsHTML then
    s := UnFixMarkup(s, false);

  Result := s;
end;

{$ENDIF}

{$IFDEF PARAMS}

function IPosv(su,s:string;var vp:integer):integer;
begin
  vp := pos(UpperCase(su),UpperCase(s));
  Result := vp;
end;


function GetHREFValue(html,href:string;var value:string):boolean;
var
  lp: Integer;
begin
  Result := False;
  while IPosv('href='+ASG_ATTR_DELIM,html,lp) > 0 do
  begin
    Delete(html,1,lp+5); {delete all before}
    if IPosv(ASG_ATTR_DELIM,html,lp) > 0 then
    begin
      if CompareText(href,copy(html,1,lp-1))=0 then
      begin
        {href match - get the value now}
        Delete(html,1,lp);
        if (iposv('>',html,lp)>0) then
        begin
          Delete(html,1,lp);
          if (iposv('<',html,lp)>0) then
          begin
            Value := Copy(html,1,lp-1);
            Result := True;
            Break;
          end;
        end;
      end;
    end;
  end;
end;


function SetHREFValue(var html:string;href,value:string):boolean;
var
  h:string;
  p:string;
begin
  {get current value and do a stringreplace}

  Result := False;
  if GetHREFValue(html,href,h) then
  begin
    p := Copy(html,pos('href="' + href,html),Length(html));

    p := StringReplace(p,'>' + h + '</A','>' + value + '</A',[rfIgnoreCase]);

    html := Copy(html,1,pos('href="'+href,html)-1)+p;
    Result := True;
  end;
end;

{$ENDIF}


procedure SetWinXP;
var
  VerInfo: TOSVersionInfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);

  IsWinXP := (verinfo.dwMajorVersion > 5) OR
    ((verinfo.dwMajorVersion = 5) AND (verinfo.dwMinorVersion >= 1));
end;


initialization
  SetWinXP;

end.
