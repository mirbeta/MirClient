{**************************************************************************}
{ Mini RTF rendering engine                                                }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 1999 - 2015                                       }
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

unit RTFEngine;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ImgList
  {$IFDEF DELPHI_UNICODE}
  , Character
  {$ENDIF}
  ;

type

  TFontItem = class(TCollectionItem)
  private
    FFaceName: String;
    procedure SetFaceName(const Value: String);
    function GetCode: String;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function GetText: String;
  published
    property FaceName: String read FFaceName write SetFaceName;
    property Code: String read GetCode;
  end;

  TFontCollection = class(TCollection)
  private
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TFontItem;
    procedure SetItem(Index: Integer; const Value: TFontItem);
  public
    constructor Create(AOwner: TComponent);
    property Items[Index: Integer]: TFontItem read GetItem write SetItem; default;
    function Add: TFontItem;
    function Insert(Index: Integer): TFontItem;
    function IndexOf(FaceName: String): Integer;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TFontTable = class(TObject)
  private
    FFonts: TFontCollection;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function AddFont(AFont: TFont): String; overload;// returns code/index, add it if not exist
    function AddFont(AFaceName: TFontName): String; overload;// returns code/index, add it if not exist
    function GetText: String;
    function IndexOf(FaceName: String): Integer;
    procedure LoadFonts(RTFString: String);
    
    property Fonts: TFontCollection read FFonts;
  end;

  TColorTable = class(TObject)
  private
    FColorList: TStringList;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function AddColor(Clr: TColor): integer; // returns code/index, add it if not exist
    function GetText: String;
    procedure LoadColors(RTFString: String);

    property Colors: TStringList read FColorList;
  end;

  TRTFHeader = class(TObject)
  private
    FFontTable: TFontTable;
    FColorTable: TColorTable;
    FViewKind: Integer;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function GetText: String;
    property FontTable: TFontTable read FFontTable;
    property ColorTable: TColorTable read FColorTable;
  end;

  TRTFEngine = class(TObject)
  private
    FRTFHeader: TRTFHeader;
    FNewLine: Boolean;
    FIsPreviouseKW: Boolean;
    FFont: TFont;
    FText: String;
    FStartTable: Boolean;
    FTableWidth: Integer;
    FStartRow: Boolean;
    FBold: Boolean;
    FItalic: Boolean;
    FUnderLine: Boolean;
    FHAlignment: TAlignment;
    FFontSize: Integer;
    FForeColor: TColor;
    FStrikeOut: Boolean;
    FStartBullet: Boolean;
    FBulletChar: Integer;
    FBulletFont: String;
    FTableAlignment: TAlignment;
    FImages: TCustomImageList;
    FCurrentCol: Integer;
    FTotalCol: Integer;
    FColColorList: TStringList;
    FHighLightColor: TColor;
    FWithBorders: Boolean;
    function ReplaceCR(s:string; dobreak:boolean):string;
  protected
    function RefreshPara(S: String; KWCode: Integer): Boolean;
    procedure AddInternal(S: String; KWCode: Integer = 0);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddFont(AFont: TFont);
    procedure AddFontName(FontName: TFontName);
    procedure AddBold(Value: Boolean);
    procedure AddItalic(Value: Boolean);
    procedure AddUnderLine(Value: Boolean);
    procedure AddStrikeOut(Value: Boolean);
    procedure AddFontSize(Value: Integer);
    procedure AddText(T: String);                   //100
    procedure AddNewLine;                           //1
    procedure AddTab;
    procedure AddParagraph(LeftInd: Integer = 720; RightInd: Integer = 0);
    procedure AddForeColor(Clr: TColor);
    procedure AddHighLightColor(Clr: TColor);
    procedure AddBackGroundColor(Clr: TColor);
    procedure AddHAlignment(Align: TAlignment);      //2
    procedure AddPageBreak;

    procedure AddHTML(S: String);
    procedure AddRTF(S: String);

    procedure AddHyperLink(Link, Text: String; AFont: TFont);

    Function AddPicture(Pic: TPicture): String;

    procedure AddSuperScript;
    procedure AddSubScript;
    procedure AddNormalScript;

    // Table
    procedure StartTable(Align: TAlignment; WithBorders: Boolean = True);          //5
    procedure EndTable;
    procedure AddColumn(ColWidth: Integer);         //6
    procedure StartRow;
    procedure EndRow;
    procedure AddRow;                               //7
    procedure NextCell;                             //8
    procedure ReDefColWidth;
    procedure AddCellColor(Clr: TColor);

    procedure StartBullet(FontName: TFontName = 'Symbol'; CharNo: Integer = 7); //11
    procedure NextBullet;                                                       //12
    procedure EndBullet;

    procedure SaveToFile(FileName: String);
    function GetText: string;
    procedure GetBuffer(var Buffer, FontTable,ColorTable: string);
    procedure SaveToStream(st: TStream);
    property Bold: Boolean read FBold;
    property Italic: Boolean read FItalic;
    property UnderLine: Boolean read FUnderLine;
    property StrikeOut: Boolean read FStrikeOut;
    property FonSize: Integer read FFontSize;
    property HAlignment: TAlignment read FHAlignment;
    property ForeColor: TColor read FForeColor;
    property HighLightColor: TColor read FHighLightColor;
    property Images: TCustomImageList read FImages write FImages;
  end;

//function HTMLToRTF(s:string; DefFont: TFont; RTFEngine: TRTFEngine): String;
//function IsHyperLink(Text: String): Boolean;
function RTFHeader: string;

implementation

const
  RTF_DEF = '\rtf1\ansi\ansicpg1252\deff0\deflang1033';

//------------------------------------------------------------------------------

function IsNumChar(ch: char): boolean;
begin
  {$IFNDEF DELPHIXE4_LVL}

  {$IFNDEF DELPHI_UNICODE}
  Result := (ch in ['0'..'9']);
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  Result := Character.IsNumber(ch);
  {$ENDIF}

  {$ENDIF}

  {$IFDEF DELPHIXE4_LVL}
  Result := ch.IsNumber;
  {$ENDIF}
end;


//------------------------------------------------------------------------------

function RemoveStartingSpace(S: String): String;
var
  i: Integer;
begin
  Result := S;
  for i := 1 to Length(s) do
  begin
    if (s[i] = ' ') then
      Result := copy(S, 2, Length(S)-1)
    else
      Break;
  end;
end;

//------------------------------------------------------------------------------

{
function IsHyperLink(Text: String): Boolean;
begin
  Result := (pos('http://', Text) > 0) or (pos('file://', Text) > 0) or (pos('ftp://', Text) > 0)
             or (pos('nntp://', Text) > 0) or (pos('https://', Text) > 0) or (pos('mailto:', Text) > 0);
end;
}

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

function CRLFStrip(s:string;dobreak:boolean):string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    if not ((s[i] = #13) or (s[i] = #10)) then
      Result := Result + s[i]
    else
      if (s[i] = #13) and dobreak then
        Result := Result + '<BR>';
  end;
end;

//------------------------------------------------------------------------------

function IPos(su,s:string):Integer;
begin
  Result := Pos(UpperCase(su),UpperCase(s));
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

function VarPos(su,s:string;var Res:Integer):Integer;
begin
  Res := Pos(su,s);
  Result := Res;
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

function Hex2Color(s:string): TColor;
var
  r,g,b: Integer;
begin
  r := Hexval(Copy(s,2,2));
  g := Hexval(Copy(s,4,2)) shl 8;
  b := Hexval(Copy(s,6,2)) shl 16;
  Result := TColor(b + g + r);
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

function IStrToInt(s:string):Integer;
var
  Err,Res: Integer;
begin
  Val(s,Res,Err);
  Result := Res;
end;

//------------------------------------------------------------------------------
{$WARNINGS OFF}
procedure HTMLToRTFEx(s:string; FImages: TCustomImageList; ShadowOffset: Integer;
                    CheckHeight,Selected,Blink,HoverStyle,WordWrap,Down: Boolean;
                    ResFactor:Double;
                    URLColor,HoverColor,HoverFontColor,ShadowColor:TColor; DefFont: TFont; RTFEngine: TRTFEngine);
var
  su: string;
  Align: TAlignment;
  //Hotspot, ImageHotspot: Boolean;
  Anchor,Error: Boolean;
  LastAnchor, AnchorText: string;
  IMGSize: TPoint;
  isSup,isSub,isPara,isShad: Boolean;
  subh,suph,imgali: Integer;
  ListIndex: Integer;
  Invisible: Boolean;
  FoundTag: Boolean;
  AltImg,ImgIdx: Integer;
  FrstBullet: Boolean;
  URLFont: TFont;
  Pic: TPicture;

  function ConvertHTMLLine(var s:string;Calc:Boolean):string;
  var
    su,Res,TagProp,Prop,{AltProp,}Tagp,LineText:string;
    cr: TRect;
    linebreak,imgbreak{,linkbreak}: Boolean;
    TagPos,SpacePos{,o,l}: Integer;
    WordLen: Integer;
    TagChar: Char;
    LengthFits{, SpaceBreak}: Boolean;
    //ControlType,ControlWidth,ControlID,ControlValue,ControlProp: string;

  begin
    Result := '';
    LineText := '';

    //sw := 0;

    LineBreak := False;
    ImgBreak := False;
    //LinkBreak := False;
    res := '';

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

      //WordLenEx := Length(su);

      if WordLen > 0 then
      begin
        //StripVal := StripVal + su;

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
            end;

          end
        else
          begin

            if Anchor then
              AnchorText := su
            else
              RTFEngine.AddText(su);
          end;

          LengthFits := True;

          LineText := LineText + su;

          if LengthFits then
          begin
            Res := Res + Copy(s,1,WordLen);
            if not LengthFits and Calc and (LineText <> su) then
              s := '';
            Delete(s,1,WordLen);
            if Length(su) >= WordLen then
            begin
             { if su[WordLen] = ' ' then
                sw := Canvas.TextWidth(' ')
              else
                sw := 0; }
            end
            {else
              sw := 0};
          end
          else
          begin
            LineBreak := True;
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
                if Anchor then
                begin
                  RTFEngine.AddHyperLink(LastAnchor, AnchorText, URLFont);
                  RTFEngine.AddFont(DefFont);
                  RTFEngine.AddForeColor(DefFont.Color);
                  if RTFEngine.FonSize <> DefFont.Size*2 then
                    RTFEngine.AddFontSize(DefFont.Size*2);
                  Anchor := False;
                end;

               { if (not HoverStyle) and not Calc then
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
                 }
              end;
          'E':begin
                if not Calc then
                  Error := False;
              end;
          'B':begin
                 if (s[4] <> '>') and (RTFEngine.ForeColor <> clBlack) then
                  RTFEngine.AddForeColor(DefFont.Color)
                else
                  RTFEngine.AddBold(False);
              end;
          'S':begin
                TagChar := UpCase(s[4]);

                if (TagChar = 'U') then
                begin
                  isSup := False;
                  isSub := False;
                end
                else
                 if (TagChar = 'H') then
                  isShad := False;
                 RTFEngine.AddStrikeOut(False); 
              end;
          'F':begin
                RTFEngine.AddFont(DefFont);
                RTFEngine.AddForeColor(DefFont.Color);
                if RTFEngine.FonSize <> DefFont.Size*2 then
                  RTFEngine.AddFontSize(DefFont.Size*2);
              end;
          'H':begin
                if not Calc then
                begin
                {  Canvas.Font.Color := hifCol;
                  Canvas.Brush.Color := hibCol;
                  if hibCol = clNone then
                    Canvas.Brush.Style := bsClear;  }
                end;
              end;
          'I':begin
                RTFEngine.AddItalic(False);
              end;
          'L':begin
                LineBreak := True;
                RTFEngine.AddNewLine;
              end;
          'P':begin
                LineBreak := True;
                if not Calc then
                begin
                  {Canvas.Brush.Color := ParaColor;
                  if ParaColor = clNone then
                    Canvas.Brush.Style := bsClear;    }
                  isPara := false;
                end;
              end;
          'U':begin
                if (s[4] <> '>') and (ListIndex > 0) then
                begin
                  RTFEngine.EndBullet;
                  FrstBullet := False;
                end
                  //Dec(Listindex)
                else
                  RTFEngine.AddUnderLine(False);
              end;
          'R':begin

              end;
          'Z':Invisible := False;
          end;
        end
        else
        begin
          case Upcase(s[2]) of
          'A':begin
                {only do this when at hover position in xpos,ypos}
              {  if (FocusLink = HyperLinks) and not Calc then
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
               }
                TagProp := Uppercase(Copy(s,3,Pos('>',s) - 1));  // <A href="anchor">

                if (VarPos('HREF',TagProp,TagPos)>0) then
                begin
                  TagProp := Copy(s,3,Pos('>',s) - 1);    // restore case ie: without upppercase

                  Prop := Copy(TagProp,TagPos + 4,Length(TagProp));
                  Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                  Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                  LastAnchor := Prop;
                  Anchor := True;
                end;
                {
                if (VarPos('TITLE',TagProp,TagPos)>0) then
                begin
                  TagProp := Copy(s,3,Pos('>',s) - 1);  // <A href="anchor">
                  Prop := Copy(TagProp,TagPos + 5,Length(TagProp));
                  Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                  Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                  AnchorHint := Prop;
                end;

                hr.Left := w;
                hr.Top := r.Top;  }
              end;
          'B':begin
                TagChar := Upcase(s[3]);
                if TagChar = '>' then  // <B> tag
                  RTFEngine.AddBold(True)
                else
                  if TagChar = 'R' then // <BR> tag
                  begin
                    LineBreak := true;
                    //StripVal := StripVal + #13;
                    RTFEngine.AddNewLine;
                  end
                  else
                  begin
                    if TagChar = 'L' then // <BLINK> tag
                    begin
                      //if not Blink then Canvas.Font.Color := BlnkColor;
                    end
                    else
                    if TagChar = 'O' then  // <BODY ... >
                    begin
                      Res := Res + Copy(s,1,pos('>',s));
                      TagProp := Uppercase(Copy(s,6,pos('>',s)-1));

                      if (Pos('BACKGROUND',TagProp) > 0) and not Calc then
                      begin
                        Prop := Copy(TagProp,Pos('BACKGROUND',TagProp)+10,Length(TagProp));
                        Prop := Copy(Prop,Pos('"',Prop)+1,Length(prop));
                        Prop := Copy(Prop,1,Pos('"',Prop)-1);

                        {bmp := nil;

                        if (Pos(':',Prop) = 0) and Assigned(pc) then
                        begin
                          bmp := pc.FindPicture(Prop);
                        end;
                        }
                        if (Pos('://',Prop) > 0){ and Assigned(ic)} then
                        begin
                         { if ic.FindPicture(Prop) = nil then
                          with ic.AddPicture do
                          begin
                            Asynch := False;
                            LoadFromURL(Prop);
                          end;

                          bmp := ic.FindPicture(Prop); }
                        end;

                       { if bmp <> Nil then
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
                        end; //end of bmp <> nil   }
                    end; //end of background

                      if (Pos('BGCOLOR',TagProp)>0) then
                      begin
                        Prop := Copy(TagProp,Pos('BGCOLOR',TagProp) + 7,Length(TagProp));
                        Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                        Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                        if not Calc then
                        begin
                          if Pos('CL',Prop) > 0 then
                            ;//Canvas.Brush.color := Text2Color(AnsiLowerCase(Prop));
                          if Pos('#',Prop) > 0 then
                            ;//Canvas.Brush.color := Hex2Color(Prop);

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
                { <CONTROL type="EDIT" width="125" ID="name" VALUE=""> }

              end;
          'H':begin
                case Upcase(s[3]) of
                'R':
                begin
                  LineBreak := True;

                end;
                'I':
                begin
                  if not Calc then
                  begin
                   { hifCol := Canvas.Font.Color;
                    hibCol := Canvas.Brush.Color;
                    if Canvas.Brush.Style = bsClear then
                      hibCol := clNone;

                    Canvas.Brush.Color := clHighLight;
                    Canvas.Font.Color := clHighLightText;
                    }
                  end;
                end;
                end;
              end;
          'I':begin
                TagChar := Upcase(s[3]);

                if TagChar = '>' then // <I> tag
                  //Canvas.Font.Style := Canvas.Font.Style + [fsItalic]
                  RTFEngine.AddItalic(True)
                else
                if TagChar = 'N' then  // <IND> tag
                begin
                  TagProp := Copy(s,3,pos('>',s)-1);

                  Prop := Copy(TagProp,ipos('x',TagProp)+2,Length(TagProp));
                  Prop := Copy(Prop,Pos('"',Prop)+1,Length(prop));
                  Prop := Copy(Prop,1,Pos('"',Prop)-1);

                  {val(Prop,indent,err);
                  if err = 0 then
                  begin
                    if Indent > w then
                     begin
                       w := Indent;
                       cr.left := fr.left + Indent;
                     end;
                  end; }
                end
                else
                  if TagChar = 'M' then
                  begin
                    inc(ImgIdx);

                    //oldfont.color:=Canvas.font.color;
                    TagProp := Uppercase(Copy(s,3,pos('>',s) - 1));
                    Prop := Copy(TagProp,Pos('SRC',TagProp) + 4,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                    if (Pos('ALT',TagProp) > 0) and (AltImg = ImgIdx) then
                    begin
                      Prop := Copy(TagProp,Pos('ALT',TagProp) + 4,Length(TagProp));
                      Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                      Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                    end;

                    //TagWidth := 0;
                    //TagHeight := 0;

                    if Pos('WIDTH',TagProp) > 0 then
                    begin
                      Tagp := Copy(TagProp,Pos('WIDTH',TagProp) + 6,Length(TagProp));
                      Tagp := Copy(Tagp,Pos('"',tagp) + 1,Length(Tagp));
                      Tagp := Copy(Tagp,1,Pos('"',tagp) - 1);
                      //Val(Tagp,TagWidth,Err);
                    end;

                    if Pos('HEIGHT',TagProp) > 0 then
                    begin
                      Tagp := Copy(TagProp,ipos('HEIGHT',TagProp) + 7,Length(TagProp));
                      Tagp := Copy(Tagp,pos('"',Tagp) + 1,Length(Tagp));
                      Tagp := Copy(Tagp,1,pos('"',Tagp) - 1);
                      //Val(Tagp,TagHeight,Err);
                    end;

                    IMGSize.x := 0;
                    IMGSize.y := 0;

                    if Pos('IDX:',Prop) > 0 then
                    begin
                      Delete(Prop,1,4);
                      if Assigned(FImages) and (IStrToInt(Prop) < FImages.Count) then
                      begin
                        Pic := TPicture.Create;
                        Pic.Bitmap.Width := FImages.Width;
                        Pic.Bitmap.Height := FImages.Height;
                        FImages.Draw(Pic.Bitmap.Canvas, 0, 0, IStrToInt(Prop));
                        RTFEngine.AddPicture(Pic);
                        Pic.Free;
                        (*IMGSize.x := MulDiv(FImages.Width,GetDeviceCaps(Canvas.Handle,LOGPIXELSX),96);
                        IMGSize.y := MulDiv(FImages.Height,GetDeviceCaps(Canvas.Handle,LOGPIXELSY),96);

                        if not Calc and not Print then
                          FImages.Draw(Canvas,cr.Left,cr.Top,IStrToInt(Prop),True);
                         *)
                      end;
                    end;

                    if Pos('SSYS:',Prop) > 0 then
                    begin
                      Delete(Prop,1,5);
                     { IMGSize := SysImage(Canvas,cr.Left,cr.Top,Prop,False,not Calc,Print,ResFactor);

                      IMGSize.x := MulDiv(IMGSize.X,GetDeviceCaps(Canvas.Handle,LOGPIXELSX),96);
                      IMGSize.y := MulDiv(IMGSize.Y,GetDeviceCaps(Canvas.Handle,LOGPIXELSY),96);  }
                    end;

                    if Pos('LSYS:',Prop) > 0 then
                    begin
                      Delete(Prop,1,5);
                     { IMGsize := SysImage(Canvas,cr.Left,cr.Top,Prop,True,not Calc,Print,ResFactor);

                      IMGSize.x := MulDiv(IMGSize.X,GetDeviceCaps(Canvas.Handle,LOGPIXELSX),96);
                      IMGSize.y := MulDiv(IMGSize.Y,GetDeviceCaps(Canvas.Handle,LOGPIXELSY),96); }
                    end;

                   { bmp := nil;

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

                      bmp := ic.FindPicture(Prop);
                    end;

                      if bmp <> nil then
                      begin
                        if not bmp.Empty then
                        begin
                          if not Calc then
                          begin

                            if (TagWidth > 0) and (TagHeight > 0) then
                            begin
                              bmp.Stretch := true;
                              Canvas.StretchDraw(Rect(cr.Left,cr.Top,cr.Left + TagWidth,cr.Top + TagHeight),bmp)
                            end
                            else
                            begin
                              // need for animation - redraw background
                              if bmp.FrameCount > 1 then
                              begin
                                Canvas.Pen.Color := BlnkColor;
                                Canvas.Brush.Color := BlnkColor;
                                Canvas.Rectangle(cr.Left,cr.Top,cr.Left + bmp.MaxWidth,cr.Top+bmp.MaxHeight);
                              end;

                              Canvas.Draw(cr.Left + bmp.FrameXPos,cr.Top + bmp.FrameYPos,bmp);
                            end;
                          end;

                          if (TagWidth > 0) and (TagHeight > 0) then
                          begin
                            IMGSize.x := MulDiv(TagWidth,GetDeviceCaps(Canvas.Handle,LOGPIXELSX),96);
                            IMGSize.y := MulDiv(TagHeight,GetDeviceCaps(Canvas.Handle,LOGPIXELSY),96);
                          end
                          else
                          begin
                            IMGSize.x := MulDiv(bmp.MaxWidth,GetDeviceCaps(Canvas.Handle,LOGPIXELSX),96);
                            IMGSize.y := MulDiv(bmp.MaxHeight,GetDeviceCaps(Canvas.Handle,LOGPIXELSY),96);
                          end;
                        end;
                      end;

                    if (XPos - r.Left > w) and (XPos - r.Left < w + IMGSize.x) and
                       (YPos > cr.Top) and (YPos < cr.Top + IMGSize.Y) and Anchor then
                    begin
                      ImageHotSpot := True;
                      AnchorVal := LastAnchor;
                      AltImg := ImgIdx;
                    end;
                      }
                    {
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
                    end;  }
                  end;
                end;
          'L':begin
                if not FrstBullet then
                  RTFEngine.AddNewLine
                else
                  FrstBullet := False;
               { w := w + 12 * ListIndex;
                if Linkbreak then
                  Imgbreak := True
                else
                  Linkbreak := True;
                }
              end;
          'U':begin
                if s[3] <> '>' then
                begin
                  Inc(ListIndex);
                  LineBreak := True;
                  RTFEngine.StartBullet;
                  FrstBullet := True;
                end
                else
                  RTFEngine.AddUnderLine(True);
              end;
          'P':begin
                if (VarPos('>',s,TagPos)>0) then
                begin
                  TagProp := Uppercase(Copy(s,3,TagPos-1));

                  if VarPos('ALIGN',TagProp,TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp,TagPos+5,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',prop)-1);

                    if Pos('RIGHT',Prop) > 0 then Align := taRightJustify;
                    if Pos('LEFT',Prop) > 0 then Align := taLeftJustify;
                    if Pos('CENTER',Prop) > 0 then Align := taCenter;
                  end;

                  if VarPos('INDENT',TagProp,TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp,TagPos+6,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',prop)-1);
                    //PIndent := IStrToInt(Prop);
                  end;


                  if VarPos('BGCOLOR',TagProp,TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp,TagPos + 5,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                   { NewColor := clNone;

                    if Length(Prop) > 0 then
                    begin
                      if Prop[1] = '#' then
                        NewColor := Hex2Color(Prop)
                      else
                        NewColor := Text2Color(AnsiLowerCase(prop));
                    end; }

                    if not Calc then
                    begin
                      isPara := True;
                      {paracolor := Canvas.Brush.Color;
                      if Canvas.Brush.Style = bsClear then ParaColor := clNone;
                      }
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
                  Prop := Copy(prop,pos('"',prop)+1,Length(prop));
                  Prop := Copy(prop,1,pos('"',prop)-1);
                  //Canvas.Font.Name := Prop;
                  RTFEngine.AddFontName(Prop);
                end;

                if (VarPos(' COLOR',TagProp,TagPos) > 0) and not Selected then
                begin
                  Prop := Copy(TagProp,TagPos+6,Length(TagProp));
                  Prop := Copy(Prop,Pos('"',prop)+1,Length(prop));
                  Prop := Copy(Prop,1,Pos('"',prop)-1);

                  if Length(Prop) > 0 then
                  begin
                    if Prop[1] = '#' then
                      //Canvas.font.color := Hex2Color(Prop)
                      RTFEngine.AddForeColor(Hex2Color(Prop))
                    else
                      RTFEngine.AddForeColor(Text2Color(AnsiLowerCase(prop)));//Canvas.Font.Color := Text2Color(AnsiLowerCase(prop));
                  end;

                end;

                if (VarPos('BGCOLOR',TagProp,TagPos)>0) and not Calc and not Selected then
                begin
                  Prop := Copy(TagProp,TagPos+7,Length(TagProp));
                  Prop := Copy(prop,pos('"',prop)+1,Length(prop));
                  Prop := Copy(prop,1,pos('"',prop)-1);
                  //BGColor := Canvas.Brush.Color;

                 { if Canvas.Brush.Style = bsClear then
                    bgcolor := clNone;
                 }
                  if Length(Prop) > 0 then
                  begin
                    if Prop[1] = '#' then
                      //Canvas.Brush.Color := Hex2Color(Prop)
                    else
                      ;//Canvas.Brush.Color := Text2Color(AnsiLowerCase(prop));
                  end;

                end;

                if (VarPos('SIZE',TagProp,TagPos)>0) then
                begin
                  Prop := Copy(TagProp,TagPos+4,Length(TagProp));
                  Prop := Copy(Prop,Pos('=',Prop)+1,Length(Prop));
                  Prop := Copy(Prop,Pos('"',Prop)+1,Length(Prop));

                  case IStrToInt(Prop) of
                  1:RTFEngine.AddFontSize(8*2);
                  2:RTFEngine.AddFontSize(10*2);
                  3:RTFEngine.AddFontSize(12*2);
                  4:RTFEngine.AddFontSize(14*2);
                  5:RTFEngine.AddFontSize(16*2);
                  else
                    RTFEngine.AddFontSize(IStrToInt(Prop)*2);
                  end;
                end;
              end;
            end;
        'S':begin
              TagChar := Upcase(s[3]);

              if TagChar = '>' then
                RTFEngine.AddStrikeOut(True)
              else
              begin
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
              prop := Copy(prop,pos('"',prop)+1,Length(prop));
              prop := Copy(prop,1,pos('"',prop)-1);
              //Val(prop,Indent,err);
              //StartRotated(Canvas,indent);
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

    {w := w - sw;

    if w > xsize then
      xsize := w;
     }
    Result := Res;
  end;

begin
  //Result := '';
  Anchor := False;
  Error := False;
  //BGColor := clNone;
  //ParaColor := clNone;
  isPara := False;
  isShad := False;
  Invisible := False;

  URLFont := TFont.Create;
  URLFont.Assign(DefFont);
  URLFont.Color := URLColor;

  //PIndent := 0;

  //HlCount := 0;
  ListIndex := 0;
  //LiCount := 0;
  //StripVal := '';

  ImgIdx := 0;
  AltImg := -1;

  if Pos('&',s) > 0 then
  begin
    repeat
      Foundtag := False;

      if TagReplacestring('&amp;','&&',s) then Foundtag := True;
      if TagReplacestring('&quot;','"',s) then Foundtag := True;

      if TagReplacestring('&sect;','§',s) then Foundtag := True;
      if TagReplacestring('&permil;','®‰',s) then Foundtag := True;
      if TagReplacestring('&reg;','®',s) then Foundtag := True;

      if TagReplacestring('&copy;','©',s) then Foundtag := True;
      if TagReplacestring('&para;','¶',s) then Foundtag := True;

      if TagReplacestring('&trade;','™',s) then Foundtag := True;
      if TagReplacestring('&euro;','€',s) then Foundtag := True;

    until not Foundtag;
  end;

  s := DBTagStrip(s);
  s := CRLFStrip(s,True);

  //InsPoint := 0;

  su := '';
  while Length(s) > 0 do
  begin
    {calculate part of the HTML text fitting on the next line}
    suph := 0;
    subh := 0;
    imgali := 0;
    isSup := False;
    isSub := False;

    //OldImgIdx := ImgIdx;

    su := su + ConvertHTMLLine(s,True);
  end;

  //Result := RTFEngine.GetText;
  URLFont.Free;
end;
{$WARNINGS ON}

//------------------------------------------------------------------------------

procedure HTMLToRTF(s:string; DefFont: TFont; RTFEngine: TRTFEngine);
begin
  HTMLToRTFEx(S, RTFEngine.Images, 0, False, False,False,False,False, False, 0, clBlue, clRed, clYellow, clGray, DefFont, RTFEngine);
end;

//------------------------------------------------------------------------------

{ TFontTable }

constructor TFontTable.Create;
begin
  inherited;
  FFonts := TFontCollection.Create(nil);
end;

//------------------------------------------------------------------------------

destructor TFontTable.Destroy;
begin
  FFonts.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TFontTable.AddFont(AFaceName: TFontName): String;
var
  i: Integer;
begin
  i := FFonts.IndexOf(AFaceName);
  if i >= 0 then
  begin
    Result := FFonts.Items[i].Code;
  end
  else
  begin
   with  FFonts.Add do
   begin
     FaceName := AFaceName;
     Result := Code;
   end;
  end;
end;

//------------------------------------------------------------------------------

function TFontTable.AddFont(AFont: TFont): String;
begin
  Result := AddFont(AFont.Name);
 { i := FFonts.IndexOf(AFont.Name);
  if i >= 0 then
  begin
    Result := FFonts.Items[i].Code;
  end
  else
  begin
   with  FFonts.Add do
   begin
     FaceName := AFont.Name;
     Result := Code;
   end;
  end;  }
end;

//------------------------------------------------------------------------------

function TFontTable.GetText: String;
var
  i: Integer;
begin
  if FFonts.Count <= 0 then
  begin
    Result := '';
    Exit;
  end;

  Result := '{\fonttbl';
  for i:= 0 to FFonts.Count-1 do
  begin
    Result := Result + FFonts.Items[i].GetText;
  end;
  Result := Result + '}';
end;

//------------------------------------------------------------------------------

function TFontTable.IndexOf(FaceName: String): Integer;
begin
  Result := FFonts.IndexOf(FaceName);
end;

//------------------------------------------------------------------------------

procedure TFontTable.LoadFonts(RTFString: String);
var
  TagPos, I, lastsp: Integer;
  fceName: String;
  AFont: TFont;
begin
// {\fonttbl{\f0\fnil Arial;}}
  TagPos := Pos('{\fonttbl', RTFString);
  if TagPos <= 0 then
    Exit;

  RTFString := copy(RTFString, TagPos + 9, Length(RTFString)-(TagPos+9));
  RTFString := RemoveStartingSpace(RTFString);

  TagPos := Pos(';}}', RTFString);
  if (RTFString[1] = '{') and (TagPos > 0) then
  begin
    AFont := TFont.Create;
    RTFString := copy(RTFString, 1, TagPos+3);

    while (Length(RTFString) > 0) do
    begin
      TagPos := Pos(';}', RTFString);
      if (TagPos <= 0) then
        Break;
      I := TagPos;
      Lastsp := -1;
      while (I > 0) do
      begin
        if RTFString[I] = ' ' then
          Lastsp := I
        else if (RTFString[I] = '\') then
        begin
          if Lastsp > 0 then
          begin
            FceName := copy(RTFString, Lastsp, TagPos - Lastsp);
            Afont.Name := Fcename;
            AddFont(AFont);
          end;
          Break;
        end;
        Dec(I);
      end;

      RTFString := copy(RTFString, TagPos + 2, Length(RTFString) - (TagPos+2));
    end;
    
    AFont.Free;
  end;

end;

//------------------------------------------------------------------------------

{ TColorTable }

function TColorTable.AddColor(Clr: TColor): integer;
var
  i: Integer;
begin
  Clr := ColorToRGB(Clr);   // FF: ie: clBtnFace 

  i := FColorList.IndexOf(ColorToString(Clr));
  if i >= 0 then
  begin
    Result := i;
  end
  else
  begin
    i := FColorList.Add(ColorToString(Clr));
    Result := i;
  end;
end;

//------------------------------------------------------------------------------

constructor TColorTable.Create;
begin
  inherited;
  FColorList := TStringList.Create;
end;

//------------------------------------------------------------------------------

destructor TColorTable.Destroy;
begin
  FColorList.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TColorTable.GetText: String;
var
  i: Integer;
begin
  if FColorList.Count <= 0 then
  begin
    Result := '';
    Exit;
  end;

  Result := '{\colortbl ';
  for i := 0 to FColorList.Count-1 do
  begin
    Result := Result + '\red'+inttostr(GetRValue(StringToColor(FColorList[i])))+'\green'+Inttostr(GetGValue(StringToColor(FColorList[i])))+'\blue'+inttostr(GetBValue(StringToColor(FColorList[i])))+';';
  end;
    Result := Result + '}'
end;

//------------------------------------------------------------------------------

procedure TColorTable.LoadColors(RTFString: String);
var
  TagPos, I: Integer;
  Sub: String;

  function RTFStringToColor(S: String): TColor;
  var
    TagPos, I : Integer;
    Sub: String;
    R, G, B: Byte;
  begin
    {Result := clNone;
    if (Pos('\red', S) <= 0) or(Pos('\green', S) <= 0) or (Pos('\blue', S) <= 0) then
      Exit; }

    R := 0;
    TagPos := Pos('\red', S);
    if TagPos > 0 then
    begin
      Sub := '';
      I := TagPos + 4;
      while (IsNumChar(S[I])) do
      begin
        Sub := Sub + S[I];
        Inc(I);
        if (I > Length(S)) then
          Break;
      end;

      R := IStrToInt(Sub);
    end;

    G := 0;
    TagPos := Pos('\green', S);
    if TagPos > 0 then
    begin
      Sub := '';
      I := TagPos + 6;
      while (IsNumChar(S[I])) do
      begin
        Sub := Sub + S[I];
        Inc(I);
        if (I > Length(S)) then
          Break;
      end;
      G := IStrToInt(Sub);
    end;

    B := 0;
    TagPos := Pos('\blue', S);
    if TagPos > 0 then
    begin
      Sub := '';
      I := TagPos + 5;
      while (IsNumChar(S[I])) do
      begin
        Sub := Sub + S[I];
        Inc(I);
        if (I > Length(S)) then
          Break;
      end;
      b := IStrToInt(Sub);
    end;

    Result := RGB(R, G, B);
  end;
begin
// {\colortbl ;\red84\green84\blue84;\red0\green0\blue0;}
  TagPos := Pos('{\colortbl', RTFString);
  if TagPos <= 0 then
    Exit;

  RTFString := copy(RTFString, TagPos + 10, Length(RTFString)-(TagPos+10));

  TagPos := Pos('}', RTFString);
  if TagPos <= 0 then
    Exit;
  Delete(RTFString, TagPos, Length(RTFString) - TagPos);

  RTFString := RemoveStartingSpace(RTFString);
  if RTFString[1] = ';' then
  begin
    RTFString := copy(RTFString, 2, Length(RTFString)-1);
    AddColor(clBlack); // default Color 
  end;

  RTFString := RemoveStartingSpace(RTFString);

  I := 1;
  while I <= Length(RTFString) do
  begin
    if RTFString[I] = '\' then
      Break
    else
      Delete(RTFString, 1, 1);
    Inc(I);
  end;

  while Length(RTFString) > 0 do
  begin
    TagPos := Pos(';', RTFString);
    if TagPos <= 0 then
      Break;

    Sub := copy(RTFString, 1, TagPos);
    AddColor(RTFStringToColor(Sub));
    Delete(RTFString, 1, TagPos);
    //RTFString := copy(RTFString, TagPos+1, Length(RTFString) - (TagPos+1));
  end;
end;

//------------------------------------------------------------------------------

{ TRTFHeader }

constructor TRTFHeader.Create;
begin
  inherited;
  FViewKind := 4;
  FFontTable := TFontTable.Create;
  FColorTable := TColorTable.Create;
end;

//------------------------------------------------------------------------------

destructor TRTFHeader.Destroy;
begin
  FFontTable.Free;
  FColorTable.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TRTFHeader.GetText: String;
begin
  Result := '{'+ RTFHeader + FFontTable.GetText + FColorTable.GetText +
            '\viewkind'+ inttostr(FViewKind)+'\uc1';
end;

//------------------------------------------------------------------------------

{ TRTFEngine }

constructor TRTFEngine.Create;
begin
  inherited;
  FRTFHeader := TRTFHeader.Create;
  FNewLine := True;
  FFont := nil;
  FText := '';
  FIsPreviouseKW := True;
  FFontSize := 12;
  FHAlignment := taLeftJustify;
  FForeColor := clBlack;
  FImages := nil;
  FCurrentCol := -1;
  FTotalCol := 0;
  FColColorList := TStringList.Create;
  FHighLightColor := clNone;
end;

//------------------------------------------------------------------------------

destructor TRTFEngine.Destroy;
begin
  FRTFHeader.Free;
  FColColorList.Free;
  if Assigned(FFont) then
    FFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TRTFEngine.RefreshPara(S: String; KWCode: Integer): Boolean;
begin
  Result := False;
  if FNewLine then
  begin
    Result := (KWCode = 2);
  end;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddInternal(S: String; KWCode: Integer);
var
  enc: string;
  ch: char;
  i: integer;
begin
  if KWCode = 100 then
  begin
    S := StringReplace(S, '\', '\\', [rfReplaceAll]);
    S := StringReplace(S, '{', '\{', [rfReplaceAll]);
    S := StringReplace(S, '}', '\}', [rfReplaceAll]);
    S := ReplaceCR(S, True);

    // encode unicode chars
    enc := '';
    for i := 1 to length(S) do
    begin
      ch := S[i];
      {$IFDEF DELPHI_UNICODE}
      if ord(ch) > 255  then
        enc := enc + '\u' + inttostr(ord(ch))+ 'G'
      else
      {$ENDIF}
       enc := enc + ch;
    end;

    S := enc;
  end;

  if RefreshPara(S, KWCode) then
  begin
    S := '\pard'+ S;
  end;

  if FIsPreviouseKW and (KWCode = 100) then       // 100 for Text
    S := ' '+ S;
  FText := FText + S;

  if (KWCode = 100) or (KWCode = 12{NextBullet}) then
    FIsPreviouseKW := False
  else
    FIsPreviouseKW := True;

  if FNewLine and (KWCode <> 1) then
    FNewLine := False;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddHAlignment(Align: TAlignment);
begin
  case Align of
    taLeftJustify : AddInternal('\ql', 2);
    taCenter      : AddInternal('\qc', 2);
    taRightJustify: AddInternal('\qr', 2);
  end;
  FHAlignment := Align;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddBackGroundColor(Clr: TColor);
begin

end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddFont(AFont: TFont);
begin
  if not Assigned(FFont) then
    FFont := TFont.Create;

  AddInternal(FRTFHeader.FontTable.AddFont(AFont));
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddForeColor(Clr: TColor);
begin
  AddInternal('\cf'+Inttostr(FRTFHeader.ColorTable.AddColor(Clr)));
  FForeColor := Clr;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddNewLine;
begin
  if FStartBullet then
    NextBullet
  else
  begin
    AddInternal('\par', 1);
    FNewLine := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddParagraph(LeftInd, RightInd: Integer);
begin
  AddInternal('\li'+inttostr(LeftInd));
  //if RightInd > 0 then
    //AddInternal('\ri'+inttostr(RightInd));
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddTab;
begin
  AddInternal('\tab');
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddText(T: String);
begin
  AddInternal(T, 100);
end;

//------------------------------------------------------------------------------

function TRTFEngine.GetText: string;
begin
  Result := FRTFHeader.GetText + FText + '\par}';
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.GetBuffer(var Buffer, FontTable,ColorTable: string);
begin
  Buffer := FText;
  FontTable := FRTFHeader.FontTable.GetText;
  ColorTable := FRTFHeader.ColorTable.GetText;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddBold(Value: Boolean);
begin
  if Value then
    AddInternal('\b')
  else
    AddInternal('\b0');
    
  FBold := Value;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddFontSize(Value: Integer);
begin
  AddInternal('\fs'+inttostr(Value));
  FFontSize := Value;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddItalic(Value: Boolean);
begin
  if Value then
    AddInternal('\i')
  else
    AddInternal('\i0');

  FItalic := Value;  
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddUnderLine(Value: Boolean);
begin
  if Value then
    AddInternal('\ul')
  else
    AddInternal('\ulnone');

  FUnderLine := Value;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddHighLightColor(Clr: TColor);
begin
  if clr = clNone then
    AddInternal('\highlight0')
  else
    AddInternal('\highlight'+ inttostr(FRTFHeader.ColorTable.AddColor(Clr)));

  FHighLightColor := Clr;    
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.SaveToFile(FileName: String);
var
  f: TextFile;
begin
  AssignFile(f, FileName);
  {$i-}
  Rewrite(f);
  {$i+}
  if IOResult <> 0 then
    raise Exception.Create('Cannot Create ' + FileName);

  Writeln(f, GetText);
  CloseFile(f);
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddColumn(ColWidth: Integer);
var
  s: string;
begin
  if FWithBorders {and (FTotalCol = 0)} then
    s := '\clbrdrl\brdrw10\brdrs\brdrcf0\clbrdrt\brdrw10\brdrs\brdrcf0\clbrdrr\brdrw10\brdrs\brdrcf0\clbrdrb\brdrw10\brdrs\brdrcf0 '
  else
    s := '';

  Inc(FTotalCol);
  FTableWidth := FTableWidth + ColWidth;
  FColColorList.AddObject(ColorToString(clWhite), Pointer(FTableWidth));
  AddInternal('\cellx'+inttostr(FTableWidth), 6);
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.EndRow;
var
  S, brs: String;
  I, ci: Integer;
begin
  if FStartRow then
  begin
    S := '';
    for I := FCurrentCol to FTotalCol-1 do
    begin
      S := S + ' \cell';
    end;

    if FWithBorders then
      brs := '\clbrdrl\brdrw10\brdrs\brdrcf0\clbrdrt\brdrw10\brdrs\brdrcf0\clbrdrr\brdrw10\brdrs\brdrcf0\clbrdrb\brdrw10\brdrs\brdrcf0'
    else
      brs := '';

    S := S + '\pard \intbl {\rtlch \af0 \ltrch \trowd \ltrrow \ts15\trgaph108\trleft-108';
    case FTableAlignment of
      taLeftJustify : S := S + '\trql';
      taCenter      : S := S + '\trqc';
      taRightJustify: S := S + '\trqr';
    end;

    if FWithBorders then
      S := S + brs;

    for I := 0 to FColColorList.Count-1 do
    begin
      if (StringToColor(FColColorList[I]) <> clWhite) then
      begin
        ci := FRTFHeader.ColorTable.AddColor(StringToColor(FColColorList[I]));
        S := S + '\clcfpat'+InttoStr(ci)+'\clshdng10000\clcfpatraw'+InttoStr(ci)+'\clshdngraw10000 ';
      end;
      S := S + ' \cellx'+ InttoStr(Integer(FColColorList.Objects[I]));
      if FWithBorders {and (i = 0)} then
        S := S + brs;
    end;

    FStartRow := False;
    FCurrentCol := -1;
    AddInternal(S + '\row }');
    //AddInternal('\row');
  end;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.StartRow;
var
  i: Integer;
begin
  FStartRow := True;
  FCurrentCol := 0;
  for i := 0 to FColColorList.Count-1 do
    FColColorList[i] := ColorToString(clWhite);
  AddInternal('\pard\intbl ', 7);
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddRow;
begin
  EndRow;
  StartRow;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.StartTable(Align: TAlignment; WithBorders: Boolean = True);
var
  s: string;
begin
  FStartTable := True;
  FTableWidth := 0;
  FTotalCol := 0;
  FColColorList.Clear;
  FTableAlignment := Align;
  FWithBorders := WithBorders;
  if FWithBorders then
    s := '\clbrdrl\brdrw10\brdrs\brdrcf0\clbrdrt\brdrw10\brdrs\brdrcf0\clbrdrr\brdrw10\brdrs\brdrcf0\clbrdrb\brdrw10\brdrs\brdrcf0 '
  else
    s := '';

  case Align of
    taLeftJustify : AddInternal('\trowd\trgaph108\trleft-108\trql' + s, 5);
    taCenter      : AddInternal('\trowd\trgaph108\trleft-108\trqc' + s, 5);
    taRightJustify: AddInternal('\trowd\trgaph108\trleft-108\trqr' + s, 5);
  end;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.NextCell;
begin
  Inc(FCurrentCol);
  AddInternal('\cell ',8);
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.EndTable;
begin
  if FStartTable then
  begin
    AddInternal('\pard\par', 5);
    FStartTable := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddFontName(FontName: TFontName);
var
  aFont: TFont;
begin
  aFont := TFont.Create;
  aFont.Name := FontName;
  AddInternal(FRTFHeader.FontTable.AddFont(aFont));
  aFont.Free;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddStrikeOut(Value: Boolean);
begin
  if Value then
    AddInternal('\strike')
  else
    AddInternal('\strike0');

  FStrikeOut := Value;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddHTML(S: String);
var
  aFont: TFont;
begin
  aFont := TFont.Create;
  HTMLToRTF(S, aFont, self);
  aFont.Free;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.EndBullet;
begin
  if FStartBullet then
  begin
    AddInternal('\par\pard\ltrpar', 11);
    FStartBullet := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.StartBullet(FontName: TFontName; CharNo: Integer);
var
  aFont: TFont;
  fi: Integer;
begin
  if not FStartBullet and (FontName <> '') then
  begin
    aFont := TFont.Create;
    aFont.Name := FontName;
    FBulletFont := FRTFHeader.FontTable.AddFont(aFont);
    fi := FRTFHeader.FontTable.IndexOf(FontName);

    AddInternal('\pard{\pntext'+FBulletFont+'\''B'+inttostr(CharNo)+'\tab}{\*\pn\pnlvlblt\pnf'+inttostr(fi)+'\pnindent0{\pntxtb\''B'+inttostr(CharNo)+'}}\ltrpar\fi-720\li720', 11);
    FStartBullet := True;
    FBulletChar := CharNo;
    aFont.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.NextBullet;
begin
  if FStartBullet then
  begin
    AddInternal('\par {\pntext'+FBulletFont+'\''B'+inttostr(FBulletChar)+'\tab}', 12);
  end;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.ReDefColWidth;
begin
  if FStartTable then
  begin
    //FTableWidth := 0;
    //AddInternal('\trowd\trgaph108\trleft-108');
    StartTable(FTableAlignment, FWithBorders);
  end;
end;

//------------------------------------------------------------------------------

function TRTFEngine.ReplaceCR(s: string; dobreak: boolean): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    if not ((s[i] = #13) or (s[i] = #10)) then
    begin
      Result := Result + s[i];
      if FNewLine then
        FNewLine := False;
    end
    else
      if (s[i] = #13) and dobreak then
      begin
       { if FStartBullet then
        begin
          Result := Result + '\par\pard\ltrpar';
          FStartBullet := False;
        end;}
        Result := Result + '\par ';
        FNewLine := True;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.SaveToStream(st: TStream);
var
  S: string;
  b: array of byte;
  i: integer;
begin
  if Assigned(st) then
  begin
    S := GetText;

    SetLength(b, length(s));

    // Extract ANSI chars from string
    {$IFDEF DELPHI_LLVM}
    for I := 0 to Length(s) - 1 do
      b[I] := ord(s[I]);
    {$ELSE}
    for i := 1 to Length(s) do
      b[I - 1] := ord(s[I]);
    {$ENDIF}

    st.Write(b[0] , Length(S));
  end;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddPageBreak;
begin
  AddInternal('\par\page ');
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddHyperLink(Link, Text: String; AFont: TFont);
var
  HyperFCode: String;
  fi, ci: Integer;
  S: String;
begin
  HyperFCode := FRTFHeader.FontTable.AddFont(aFont);
  fi := FRTFHeader.FontTable.IndexOf(aFont.Name);
  ci := FRTFHeader.ColorTable.AddColor(aFont.Color);
  S := '{\field{\*\fldinst {\rtlch \af'+inttostr(fi)+'\afs'+inttostr(aFont.Size*2)+'\ltrch \f'+inttostr(fi)+'\fs'+inttostr(aFont.Size*2)+'  HYPERLINK "'+
        Link+'" }}{\fldrslt {\rtlch \af'+inttostr(fi)+'\afs'+inttostr(aFont.Size*2)+' \ltrch \f'+inttostr(fi)+'\fs'+inttostr(aFont.Size*2)+'\ul\cf'+inttostr(ci)+' '+Text+'}}}';

  FText := FText + S;
end;

//------------------------------------------------------------------------------

function TRTFEngine.AddPicture(Pic: TPicture): String;
const
  IMAGE_DIVIDER = 25.3;
var
  Graphic: TGraphic;
  Str: TMemoryStream;
  j, fx, fy, dx, dy, n, n1: Integer;
  s, s0, s1: String;
  CellsLine, PicString: String;
  //CellsStream: TStream;
  bArr: array[0..1023] of Byte;
  bmp: TBitmap;
  R: TRect;
  //FS: TFileStream;
begin
  if not Assigned(Pic) then
    Exit;

  bmp := TBitMap.Create;
  try
    bmp.Width := Pic.Width;
    bmp.Height := Pic.Height;
    if (bmp.Height > 150) or (bmp.Width > 150) then
    begin
      if (bmp.Height > 150) then
        bmp.Height := 150;
      if (bmp.Width > 150) then
        bmp.Width := 150;

      R := Rect(0, 0, bmp.Width, bmp.Height);
      bmp.Canvas.StretchDraw(R, Pic.Graphic);
    end
    else
      bmp.Canvas.Draw(0, 0, Pic.Graphic);
    PicString := '';
    Result := '';
    Graphic := bmp;
    if not ((Graphic = nil) or Graphic.Empty) then
    begin
      Str := TMemoryStream.Create;
      //CellsStream := TMemoryStream.Create;
      try
        dx := Round(bmp.Width);
        dy := Round(bmp.Height);
        fx := Graphic.Width;
        fy := Graphic.Height;
        Graphic.SaveToStream(Str);
        Str.Position := 0;
        CellsLine := '{\sb0\li0\sl0\slmult0 {\pict\wmetafile8\picw' + FloatToStr(Round(dx * IMAGE_DIVIDER)) +
             '\pich' + FloatToStr(Round(dy * IMAGE_DIVIDER)) + '\picbmp\picbpp4' + #13#10;
        //CellsStream.Write(CellsLine[1], Length(CellsLine));
        PicString := PicString + CellsLine;
        Str.Read(n, 2);
        Str.Read(n, 4);
        n := n div 2 + 7;
        s0 := IntToHex(n + $24, 8);
        s := '010009000003' + Copy(s0, 7, 2) + Copy(s0, 5, 2) +
             Copy(s0, 3, 2) + Copy(s0, 1, 2) + '0000';
        s0 := IntToHex(n, 8);
        s1 := Copy(s0, 7, 2) + Copy(s0, 5, 2) + Copy(s0, 3, 2) + Copy(s0, 1, 2);
        s := s + s1 + '0000050000000b0200000000050000000c02';
        s0 := IntToHex(fy, 4);
        s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2);
        s0 := IntToHex(fx, 4);
        s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2) +
          '05000000090200000000050000000102ffffff000400000007010300' + s1 +
          '430f2000cc000000';
        s0 := IntToHex(fy, 4);
        s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2);
        s0 := IntToHex(fx, 4);
        s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2) + '00000000';
        s0 := IntToHex(fy, 4);
        s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2);
        s0 := IntToHex(fx, 4);
        s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2) + '00000000';
        CellsLine := s + #13#10;
        //CellsStream.Write(CellsLine[1], Length(CellsLine));
        PicString := PicString + CellsLine;
        Str.Read(bArr[0], 8);
        n1 := 0; s := '';
        repeat
          n := Str.Read(bArr[0], 1024);
          for j := 0 to n - 1 do
          begin
            s := s + IntToHex(bArr[j], 2);
            Inc(n1);
            if n1 > 63 then
            begin
              n1 := 0;
              CellsLine := s + #13#10;
              //CellsStream.Write(CellsLine[1], Length(CellsLine));
              PicString := PicString + CellsLine;
              s := '';
            end;
          end;
        until n < 1024;
      finally
        Str.Free;
      end;
      if n1 <> 0 then
      begin
        CellsLine := s + #13#10;
        //CellsStream.Write(CellsLine[1], Length(CellsLine));
        PicString := PicString + CellsLine;
      end;
      s := '030000000000}';
      CellsLine := s + '}' + #13#10;
      //CellsStream.Write(CellsLine[1], Length(CellsLine));
      PicString := PicString + CellsLine;

      FText := FText + PicString;
      Result := PicString;
      //if CellsStream.Size > 0 then
      begin
        try
          //FS := TFileStream.Create('xyz.rtf', fmCreate);
          //FS.CopyFrom(CellsStream, 0);
          //FS.Free;
        except
        end;
      end;
      //CellsStream.Free;
    end;
  finally
    bmp.Free;
  end;
end;

(*var
  bi, bb, rtf: string;
  bis, bbs: Cardinal;
  achar: ShortString;
  hexpict: string;
  I: Integer;
begin
  GetDIBSizes(pict.Handle, bis, bbs);
  SetLength(bi, bis);
  SetLength(bb, bbs);
  GetDIB(pict.Handle, pict.Palette, PChar(bi)^, PChar(bb)^);
  rtf := '{\rtf1 {\pict\dibitmap ';
  SetLength(hexpict, (Length(bb) + Length(bi)) * 2);
  I := 2;
  for bis := 1 to Length(bi) do
  begin
    achar := Format('%x', [Integer(bi[bis])]);
    if Length(achar) = 1 then
      achar := '0' + achar;
    hexpict[I - 1] := achar[1];
    hexpict[I] := achar[2];
    Inc(I, 2);
  end;
  for bbs := 1 to Length(bb) do
  begin
    achar := Format('%x', [Integer(bb[bbs])]);
    if Length(achar) = 1 then
      achar := '0' + achar;
    hexpict[I - 1] := achar[1];
    hexpict[I] := achar[2];
    Inc(I, 2);
  end;
  rtf := rtf + hexpict + ' }}';
  Result := rtf;
  FText := FText + Result;
end;
*)
(*var
  x,y : Integer;
  P : PByteArray;
begin
  Result := '{\object\objemb{\*\objclass Paint.Picture}\objw194\objh194{\*\objdata ';
  bmp.PixelFormat := pf8Bit;
  bmp.Transparent := True;
  for y := 0 to bmp.Height -1 do
  begin
    P := bmp.ScanLine[y];
    for x := 0 to bmp.Width -1 do
      Result := Result + inttostr(P[x]);
      //P[x] := y;
  end;
  Result := Result + '}}';
  FText := FText + Result;
end;*)

{var
  SF: TStringStream;
begin
  SF := TStringStream.Create('');
  bmp.SaveToStream(SF);
  Result := SF.DataString;
  SF.Free;


    bmp.Canvas.Pixels[0, 0]
  BinStream := TMemoryStream.Create;
  try
    StrStream := TStringStream.Create('');
    try
      BinStream.WriteComponent(Component);
      BinStream.Seek(0, soFromBeginning);
      ObjectBinaryToText(BinStream, StrStream);
      StrStream.Seek(0, soFromBeginning);
      Result:= StrStream.DataString;
    finally
      StrStream.Free;

    end;
  finally
    BinStream.Free
  end;

end; }

//------------------------------------------------------------------------------
(*
function TRTFEngine.AddBitMap2(Pict: TBitMap): String;
const
  IMAGE_DIVIDER = 25.3;
var
  Graphic: TGraphic;
  Str: TMemoryStream;
  i, j, x, y, fx, fy, dx, dy, n, n1, pbk: Integer;
  dcol, drow, xoffs: Integer;
  buff, s, s0, s1, s2: String;
  CellsLine: String;
  CellsStream: TStream;
  bArr: array[0..1023] of Byte;
  FS: TFileStream;
begin
  CellsStream := TStream.Create;
  Graphic := Pict;
  if not ((Graphic = nil) or Graphic.Empty) then
  begin
    Str := TMemoryStream.Create;
    CellsStream := TMemoryStream.Create;
    try
      dx := Round(Pict.Width);
      dy := Round(Pict.Height);
      fx := Graphic.Width;
      fy := Graphic.Height;
      Graphic.SaveToStream(Str);
      Str.Position := 0;
      CellsLine := '{\sb0\li0\sl0\slmult0 {\pict\wmetafile8\picw' + FloatToStr(Round(dx * IMAGE_DIVIDER)) +
           '\pich' + FloatToStr(Round(dy * IMAGE_DIVIDER)) + '\picbmp\picbpp4' + #13#10;
      CellsStream.Write(CellsLine[1], Length(CellsLine));
      Str.Read(n, 2);
      Str.Read(n, 4);
      n := n div 2 + 7;
      s0 := IntToHex(n + $24, 8);
      s := '010009000003' + Copy(s0, 7, 2) + Copy(s0, 5, 2) +
           Copy(s0, 3, 2) + Copy(s0, 1, 2) + '0000';
      s0 := IntToHex(n, 8);
      s1 := Copy(s0, 7, 2) + Copy(s0, 5, 2) + Copy(s0, 3, 2) + Copy(s0, 1, 2);
      s := s + s1 + '0000050000000b0200000000050000000c02';
      s0 := IntToHex(fy, 4);
      s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2);
      s0 := IntToHex(fx, 4);
      s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2) +
        '05000000090200000000050000000102ffffff000400000007010300' + s1 +
        '430f2000cc000000';
      s0 := IntToHex(fy, 4);
      s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2);
      s0 := IntToHex(fx, 4);
      s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2) + '00000000';
      s0 := IntToHex(fy, 4);
      s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2);
      s0 := IntToHex(fx, 4);
      s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2) + '00000000';
      CellsLine := s + #13#10;
      CellsStream.Write(CellsLine[1], Length(CellsLine));
      Str.Read(bArr[0], 8);
      n1 := 0; s := '';
      repeat
        n := Str.Read(bArr[0], 1024);
        for j := 0 to n - 1 do
        begin
          s := s + IntToHex(bArr[j], 2);
          Inc(n1);
          if n1 > 63 then
          begin
            n1 := 0;
            CellsLine := s + #13#10;
            CellsStream.Write(CellsLine[1], Length(CellsLine));
            s := '';
          end;
        end;
      until n < 1024;
    finally
      Str.Free;
    end;
    if n1 <> 0 then
    begin
      CellsLine := s + #13#10;
      CellsStream.Write(CellsLine[1], Length(CellsLine));
    end;
    s := '030000000000}';
    CellsLine := s + '\cell}' + #13#10;
    CellsStream.Write(CellsLine[1], Length(CellsLine));

    if CellsStream.Size > 0 then
    begin
      try
        FS := TFileStream.Create('xyz.rtf', fmCreate);
        FS.CopyFrom(CellsStream, 0);
        FS.Free;
      except
      end;
    end;

  end;
end;
*)

//------------------------------------------------------------------------------

procedure TRTFEngine.AddCellColor(Clr: TColor);
begin
  if FStartRow and (FCurrentCol >= 0) then
  begin
    //while FCurrentCol >= FColColorList.Count do
      //FColColorList.Add(ColorToString(clWhite));
      
    if FCurrentCol < FColColorList.Count then
      FColColorList[FCurrentCol] := ColorToString(Clr);
      
  end;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddNormalScript;
begin
  AddInternal('\up0\nosupersub');
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddSubScript;
begin
  AddInternal('\sub\dn4');
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddSuperScript;
begin
  AddInternal('\super\up4');
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddRTF(S: String);
var
  AFontTable: TFontTable;
  AColorTable: TColorTable;
  I, Braces: Integer;
  Tag, NewRTF: String;
  Tags: TStringList;

  function RemoveRTFDef(S: String): String;
  var
    TagPos, I: Integer;
    Sub: String;
  begin
    Result := S;
    if Pos('{\rtf1', S) <= 0 then
      Exit;

    TagPos := Pos('\viewkind', S);
    if (TagPos > 0) then
    begin
      Result := copy(S, TagPos + 10, Length(S)-TagPos-9);

      Sub := copy(Result, 1, 4);
      //TagPos := Pos('\uc1', Sub);
      if (Sub = '\uc1') then
        Delete(Result, 1, 4);
        //Result := copy(Result, 1, TagPos);

      I := Length(Result);
      while (I > 0) do
      begin
        if (Result[I] = '}') then
        begin
          Result := copy(Result, 1, I-1);
          Break;
        end;
        Dec(I);
      end;
    end;
  end;

  function AcceptableTag(Tag: String): String;
  var
    Sub: String;
    I, Val, j: Integer;
  begin
    if (Tags.IndexOf(Tag) >= 0) then
      Result := Tag
    else
    begin
      Result := '';

      Sub := '';
      I := 1;
      while (I <= Length(Tag)) do
      begin
        if (IsNumChar(Tag[I])) then
        begin
          Sub := Sub + Tag[I];
          Delete(Tag, I, 1);
          Dec(I);
        end
        else
        begin
          if (Sub <> '') then
            Exit;
        end;

        Inc(I);
      end;
      Val := IStrToInt(Sub);

      if (Tag = '\cf') or (Tag = 'highlight') then
      begin
        if (Val >= 0) and (Val < AColorTable.Colors.Count) then
        begin
          Result := Tag + IntToStr(FRTFheader.ColorTable.AddColor(StringToColor(AColorTable.Colors[Val])));
        end;
      end
      else if (Tag = '\f') or (Tag = '\pnf') then
      begin
        if (Val >= 0) and (Val < AFontTable.Fonts.Count) then
        begin
          if (Tag = '\f') then
            Result := FRTFheader.FontTable.AddFont(AFontTable.Fonts[Val].FaceName)
          else
          begin
            j := FRTFheader.FontTable.IndexOf(AFontTable.Fonts[Val].FaceName);
            if (j < 0) then
            begin
              FRTFheader.FontTable.AddFont(AFontTable.Fonts[Val].FaceName);
              j := FRTFheader.FontTable.IndexOf(AFontTable.Fonts[Val].FaceName);
            end;
            Result := Tag + IntToStr(j);
          end;
        end;
      end
      else if (Tag = '\fs') or (Tag = '\li') or (Tag = '\fi-') then
      begin
        Result := Tag + Sub;
      end;

    end;
  end;

  procedure AddTag;
  begin
    if (Tag <> '') then
    begin
      NewRTF := NewRTF + AcceptableTag(Tag);

      Tag := '';
    end;
  end;

begin
  AFontTable := TFontTable.Create;
  AFontTable.LoadFonts(S);
  AColorTable := TColorTable.Create;
  AColorTable.LoadColors(S);

  Tags := TStringlist.Create;
  if not FStartTable then
    Tags.Add('\pard');
  Tags.Add('\ql'); Tags.Add('\qc');
  Tags.Add('\qr'); Tags.Add('\par'); Tags.Add('\tab');
  Tags.Add('\b'); Tags.Add('\b0'); Tags.Add('\i');
  Tags.Add('\i0'); Tags.Add('\ul'); Tags.Add('\ulnone');
  Tags.Add('\highlight0'); Tags.Add('\strike'); Tags.Add('\strike0');
  Tags.Add('\ltrpar'); Tags.Add('\pntext'); Tags.Add('\pnlvlblt');
  Tags.Add('\pnindent0'); Tags.Add('\pntxtb');
  Tags.Add('\''B7'); Tags.Add('\*'); Tags.Add('\pn');
  Tags.Add('\up0'); Tags.Add('\dn4'); Tags.Add('\up4');
  Tags.Add('\page'); Tags.Add('\li720'); Tags.Add('\fi-720');
  
  {
  //--- Add font
  for I:= 0 to AFontTable.Fonts.Count-1 do
  begin
    FRTFHeader.FontTable.AddFont(AFontTable.Fonts[I].FaceName);
  end;
  //--- Add Color
  for I:= 0 to AColorTable.Colors.Count-1 do
  begin

  end;  }

  //--- Remove RTF definition
  S := RemoveRTFDef(S);
  //S := RemoveStartingSpace(S);

  I := 1;
  Tag := '';
  Braces := 0;
  NewRTF := '';
  while (I <= Length(s)) do
  begin

    case S[I] of
      '\':
      begin
        AddTag;
        Tag := '\';
      end;
      ' ':
      begin
        AddTag;
        NewRTF := NewRTF + S[I];
      end;
      '{':
      begin
        AddTag;
        Inc(Braces);
        NewRTF := NewRTF + S[I];
      end;
      '}':
      begin
        AddTag;
        Dec(Braces);
        NewRTF := NewRTF + S[I];
      end;
      {'0','1','2','3','4','5','6','7','8','9':
      begin

      end}
      else
      begin
        if (Tag <> '') then
          Tag := Tag + S[I]
        else
          NewRTF := NewRTF + S[I];
      end;
    end;


    Inc(I);
  end;

  if (Braces <> 0) then
  begin

  end;

  FText := FText + NewRTF;
  AFontTable.Free;
  AColorTable.Free;
  Tags.Free;
end;

//------------------------------------------------------------------------------

{ TFontCollection }

function TFontCollection.Add: TFontItem;
begin
  Result := TFontItem(inherited Add);
end;

//------------------------------------------------------------------------------

constructor TFontCollection.Create(AOwner: TComponent);
begin
  inherited Create(TFontItem);
end;

//------------------------------------------------------------------------------

function TFontCollection.GetItem(Index: Integer): TFontItem;
begin
  Result := TFontItem(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

function TFontCollection.IndexOf(FaceName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i:= 0 to Self.Count-1 do
  begin
    if UpperCase(Items[i].FaceName) = UpperCase(FaceName) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TFontCollection.Insert(Index: Integer): TFontItem;
begin
  Result := TFontItem(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TFontCollection.SetItem(Index: Integer; const Value: TFontItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

{ TFontItem }

constructor TFontItem.Create(Collection: TCollection);
begin
  inherited;

end;

//------------------------------------------------------------------------------

destructor TFontItem.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------

function TFontItem.GetCode: String;
begin
  Result := '\f'+ inttostr(Index);
end;

//------------------------------------------------------------------------------

function TFontItem.GetText: String;
begin
  Result := '{'+Code+'\fnil '+FaceName+';}';
end;

//------------------------------------------------------------------------------

procedure TFontItem.SetFaceName(const Value: String);
begin
  FFaceName := Value;
end;

//------------------------------------------------------------------------------

function RTFHeader: string;
begin
  Result := RTF_DEF;
end;

end.
