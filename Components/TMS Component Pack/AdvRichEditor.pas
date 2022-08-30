{*************************************************************************}
{ TMS TAdvRichEditor                                                      }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2014 - 2015                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvRichEditor;

{$I TMSDEFS.INC}

interface

//{$DEFINE TMSDEBUG}

uses
  Classes, Types, Graphics, Windows, Messages, Controls,
  Dialogs, Clipbrd, Forms, ExtCtrls, Menus,
  ImgList, Printers, ActnList, Generics.Collections,
  AdvRichEditorBase, AdvRichEditorRTF
  {$IFNDEF LCLLIB}
  , AdvPicture, AdvToolBarRes, GDIPicture, AdvGDIP, JPEG, GIFImg, PngImage
  , AdvRichEditorDD
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes, AnsiStrings
  {$ENDIF}
  {$IFDEF TMSPACK}
  , GDIPPictureContainer
  {$ENDIF}
  {$IFDEF TMSDEBUG}
  , TMSLogger
  {$ENDIF}
  {$IFDEF LCLLIB}
  , LMessages, LCLTMSRichEditorUtil
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 8; // Release nr.
  BLD_VER = 1; // Build nr.
  DATE_VER = 'Oct, 2015'; // Month version

  // version history
  // v1.0.0.0 : First release
  // v1.0.1.0 : New : Public functions
  //                  ContentAsHTML, ContentAsRTF, SelectionAsHTML, SelectionAsRTF
  //          : Fixed : Issue with changing formatting when editor has no selection
  // v1.0.1.1 : Fixed : Issue with retrieving font settings for selected text in the editor
  // v1.0.1.2 : Fixed : Issue with setting color, background color when no text is selected
  // v1.0.2.0 : New : Method SetCaret() added
  //          : Fixed : Rare issue with bullet insertion
  //          : Improved : Unicode character export to RTF
  // v1.0.2.1 : Fixed: Issue with inserting image after bullet list in very specific circumstances
  // v1.0.3.0 : New : OnCorrectWord event added
  //          : New : URLAuto property added
  //          : New : Method ClearErrors added
  //          : New : SelStart, SelLength properties added
  //          : Improved : Font handling during HTML export
  //          : Improved : Handling large text paste
  // v1.0.3.1 : Fixed : Issue with background colors for HTML export
  // v1.1.0.0 : New : AutoCorrect
  //          : New : Emoticons support
  //          : New : Support for PictureContainer images
  //          : New : OLE drag & drop support
  //          : New : AppendFile() method added
  //          : New : Interface to spell check engine
  //          : New : DB-aware version
  //          : New : TAdvRichEditorMiniHTMLIO component
  // v1.1.0.1 : Fixed : Issue with inserting text after merge fields
  //          : Fixed : Issue with spell check dialog initialization
  // v1.1.1.0 : Improved : HTML import
  //          : New : Public property ZoomFactor
  // v1.1.1.1 : Fixed : Issue with HTML generation
  // v1.1.1.2 : Improved : HTML compliance for export
  //          : Improved : Selection of graphic elements
  // v1.2.0.0 : New : Popup mini format toolbar
  //          : New : Export of custom graphic elements to PDF
  //          : New : TAdvRichEditorMiniHTMLIO.Insert(HTMLValue) added to insert snippets of HTML in richeditor
  //          : Improved : Quality of PDF export
  //          : Improved : Behavior of UnMerge
  // v1.2.0.1 : Fixed : Issue with hiding popup toolbar in specific circumstances
  // v1.2.0.2 : Fixed : Rare issue with changing background color
  //          : Improved : Auto URL detection with pasting text from clipboard
  // v1.2.0.3 : Fixed : Rare issue with inserting text after loading file
  //          : Fixed : Issue with spell check & inserting line break at specific positions
  //          : Fixed : Issue with handling return key on modal forms
  // v1.2.1.0 : New : Support for Delphi XE8 & C++Builder XE8 Prof, Ent. Architect added
  // v1.2.1.1 : Fixed : Issue with clipboard support
  // v1.3.0.0 : New : Support for VCL Styles added
  // v1.3.0.1 : Fixed : Issue with ReplaceAll() in specific circumstances
  //          : Fixed : Issue with persisting merge fields on stream
  // v1.3.1.0 : New : Event OnCanSelectGraphic, OnCanSizeGraphic added
  //          : New : Event OnClickGraphic added
  //          : New : Support for pasting basic HTML from clipboard
  // v1.3.2.0 : New : Overload for LoadFromTextFile() with Encoding parameter
  // v1.3.3.0 : New : Enhanced URLAuto handling
  // v1.3.3.1 : Fixed : Issue with char encoding when pasting from HTML
  //          : Improved : Pasting for HTML formatted text
  // v1.3.3.2 : Fixed : Issue with Modified flag handling
  // v1.3.3.3 : Fixed : Caret positioning issue in center or right aligned text
  // v1.4.0.0 : New : RTF import
  //          : New : RTF clipboard functionality
  //          : Misc : Various smaller improvements & fixes
  // v1.4.0.1 : Fixed : Issue with toolbar font picker style
  //          : Fixed : Issue with TAdvRichEditorEditingToolbar button options
  //          : Fixed : Issue with repeated copy to clipboard in RTF format
  // v1.4.0.2 : Improved : TAdvRichEditorEditToolBar Open function now has option to open RTF file as well
  //          : Fixed: Issue with paste when caret/selection is on linebreak
  //          : Improved : RTF parser compatibility with OpenOffice & LibreOffice RTF
  // v1.4.1.0 : New : function GetContentAsRTE(Selection): string added
  //          : New : property ContentAsRTE property added
  // v1.4.1.1 : Improved : Sizing of images with aspect ratio
  //          : Fixed : Issue with getting selection attributes after RTF import
  //          : Fixed : Issue with InsertMultiLineText with linebreaks
  // v1.4.2.0 : New : Property AllowSelect added
  //          : New : Drag & drop of RTF files
  //          : Improved : Insert of multiline text
  // v1.4.3.0 : New : TAdvRichEditorIO.Load() overload added with stream parameter
  // v1.4.3.1 : Fixed : Issue with find & replace
  //          : Fixed : Issue with VCL styles & scrollbar
  //          : Fixed : Issue with VCL styles and bullet color
  // v1.4.4.0 : New : Support to export to HTML with inline images
  //          : Improved : HTML import
  // v1.4.4.1 : Fixed : Issue with ReplaceAll under specific circumstances
  //          : Fixed : Issue with image drag & drop
  // v1.4.4.2 : Fixed : RTF import issue for specific codepage settings
  // v1.4.4.3 : Fixed : Issue with RTF parsing when RTF contains tabs
  // v1.4.5.0 : New : RAD Studio 10 Seattle support
  // v1.4.5.1 : Improved : HTML Import and export support
  // v1.4.6.0 : Improved : Default font handled for HTML export
  //          : Fixed : Rare issue with HTML export with URLs
  // v1.4.6.1 : Improved : RTF font color import handling
  // v1.4.7.0 : New : GotoTextBegin, GotoTextEnd, GotoLineBegin and GotoLineEnd
  //          : Fixed : Caret issue during insert hyperlink
  // v1.4.8.0 : New : Import of JPEGBLIP images from RTF files
  //          : New : Public property .ClipboardFormats to allow to exclude specific formats from pasting
  //          : New : OnPasteText event , OnPasteFormattedText event
  //          : New : Public property BulletSpacing added
  //          : Improved RTF attributes import
  //          : Improved RTF unicode font import
  //          : Improved RTF clipboard paste
  //          : Fixed : parsing of subscript / superscript from RTF files
  // v1.4.8.1 : Fixed : Issue with RTF color table import


type
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;

  TAdvRichEditor = class;

  TActionClass = class of TAction;

  TRichEditorDrawGraphicEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AID: string) of object;

  THyperlinkClickEvent = procedure(Sender: TObject; URL: string) of object;

  TGraphicClickEvent = procedure(Sender: TObject; AGraphic: TREElement) of object;

  TGraphicAllowEvent = procedure(Sender: TObject; AGraphic: TREElement; var Allow: boolean) of object;

  TWordContextEvent = procedure(Sender: TObject; MousePos: TPoint; AWord: string; AElement: TREElement; var Handled: boolean) of object;

  TPasteTextEvent = procedure(Sender: TObject; var AText: string; var Allow: boolean) of object;

  TPasteFormattedTextEvent = procedure(Sender: TObject; AStream: TStream) of object;

  TImageResizeCorner = (irTopLeft,irTopRight,irBottomLeft,irBottomRight);

  TGraphicSelectionStyle = (gsRect, gsCircle);

  TClipboardFormat = (cfRTE, cfRTF, cfText, cfHTML, cfBMP, cfFile);

  TClipboardFormats = set of TClipboardFormat;

  {$IFNDEF TMSPACK}
  TGDIPPictureContainer = class(TComponent);
  {$ENDIF}

  TGraphicSelection = class(TPersistent)
  private
    FBorderColor: TColor;
    FColor: TColor;
    FStyle: TGraphicSelectionStyle;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Style: TGraphicSelectionStyle read FStyle write FStyle default gsRect;
    property Color: TColor read FColor write FColor default clWhite;
    property BorderColor: TColor read FBorderColor write FBorderColor default clGray;
  end;

  {$IFNDEF LCLLIB}
  TAdvRichEditorDropTarget = class(TRichEditorDropTarget)
  private
    FRichEditor: TAdvRichEditor;
  public
    constructor Create(AEditor: TAdvRichEditor);
    procedure DropText(pt: TPoint;s: string; dwEffect: longint); override;
    procedure DropStream(pt: TPoint; AStream: TMemoryStream; dwEffect: longint); override;
    procedure DropRTF(pt: TPoint;s: string; dwEffect: longint); override;
    procedure DropFiles(pt: TPoint;Files: TStrings; dwEffect: longint); override;
    procedure DropURL(pt: TPoint; s: string; dwEffect: longint); override;
    procedure DropBMP(pt: TPoint; bmp: Graphics.TBitmap; dwEffect: longint); override;
    procedure DragMouseMove(pt:TPoint; var Allow: Boolean; DropFormats: TDropFormats); override;
    procedure DragMouseLeave; override;
  end;

  TAdvRichEditorDropSource = class(TRichEditorDropSource)
  private
    FRichEditor: TAdvRichEditor;
    FLastEffect: Integer;
  protected
    procedure DragDropStop; override;
  public
    constructor Create(AEditor: TAdvRichEditor);
    procedure CurrentEffect(dwEffect: Longint); override;
    procedure QueryDrag; override;
    property LastEffect: Integer read FLastEffect;
  end;
  {$ENDIF}

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvRichEditorPopup = class(TComponent)
  private
    FRichEditor: TAdvRichEditor;
    FVisible: boolean;
  protected
    property RichEditor: TAdvRichEditor read FRichEditor write FRichEditor;
  public
    procedure Show(PT: TPoint); virtual;
    procedure Hide; virtual;
    function MouseInPopup(PT: TPoint): boolean; virtual; abstract;
    property Visible: boolean read FVisible;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvRichEditor = class(TAdvRichEditorBase)
  private
  {$IFNDEF LCLLIB}
    FDropTarget: TAdvRichEditorDropTarget;
    FDropSource: TAdvRichEditorDropSource;
  {$ENDIF}
  {$IFDEF TMSPACK}
    FGDIPPicture: TGDIPPicture;
    FGDIPPictureContainer: TGDIPPictureContainer;
    FEmoticons: TGDIPPictureContainer;
  {$ENDIF}
    FDropTargetAssigned: boolean;
    FMouseDown: boolean;
    FDownPos: TPoint;
    FDownXY: TPoint;
    FDownSize: TSize;
    FCaretTimer: TTimer;
    FDoCaret: boolean;
    FDoImageResize: boolean;
    FImageResizeCorner: TImageResizeCorner;
    FPopupMenu: TPopupMenu;
    FImageList: TImageList;
    FOnDrawGraphic: TRichEditorDrawGraphicEvent;
    FOnClickHyperlink: THyperlinkClickEvent;
    FOnClickGraphic: TGraphicClickEvent;
    FOnContextForWord: TWordContextEvent;
    FOnCanSelectGraphic: TGraphicAllowEvent;
    FOnCanSizeGraphic: TGraphicAllowEvent;
    FClearAction: TAdvRichEditorClear;
    FCutAction: TAdvRichEditorCut;
    FCopyAction: TAdvRichEditorCopy;
    FPasteAction: TAdvRichEditorPaste;
    FLeftAction: TAdvRichEditorAlignLeft;
    FCenterAction: TAdvRichEditorAlignCenter;
    FRightAction: TAdvRichEditorAlignRight;
    FGraphicSelection: TGraphicSelection;
    FLastHint: string;
    FClickOnSel: boolean;
    FClickSelXY: TPoint;
    FInternalDD: boolean;
    FZoomFactor: double;
    FSingleLine: boolean;
    FRichEditorPopup: TAdvRichEditorPopup;
    FAllowSelect: boolean;
    FClipboardFormats: TClipboardFormats;
    FOnPasteText: TPasteTextEvent;
    FOnPasteFormattedText: TPasteFormattedTextEvent;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    {$IFDEF DELPHIXE2_LVL}
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    {$ENDIF}
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    procedure SetGraphicSelection(const Value: TGraphicSelection);
    procedure SetZoomFactor(const Value: double);
  protected
    function GetTextDescent(ACanvas: TCanvas; el: TREElement): integer; override;
    function GetTextSize(ACanvas: TCanvas; el: TREElement; AValue: string): TTextSize; override;
    function GetBulletSize(el: TREElement; AValue: string): TSize; override;
    function GetPictureSize(el: TREElement): TSize; override;
    function GetCharInWord(el: TREElement; s: string; X: integer): integer; override;
    function GetCharPos(AValue: string; CharIndex: integer): integer; override;
    function GetDefaultFont: TFont; override;
    function GetDefaultFontColor: TColor; override;
    function GetLineHeight(AElement: TREElement): integer; override;
    function GetClientWidth: integer; override;
    function GetClientHeight: integer; override;
    function GetBulletChar(const AType: TBulletType): char; override;

{$IFDEF TMSPACK}
    function GetPictureByName(Container: TGDIPPictureContainer; const AName: string): TGDIPPicture;
{$ENDIF}
    procedure DrawErrorLine(ACanvas: TCanvas; x,w,y: integer; AColor: TColor);
    procedure DrawElement(ACanvas: TCanvas; el: TREElement; x, y, MaxLineWidth, LineHeight, LineDescent, LineBaseLine: integer; AValue: string; CharIndex: integer); override;
    procedure DrawSelection(ACanvas: TCanvas; r: TRect); override;
    procedure DrawCaret(ACanvas: TCanvas); override;
    procedure DrawDragCaret(ACanvas: TCanvas); override;
    procedure DrawBackground(ACanvas: TCanvas); override;
    procedure DrawMargin(ACanvas: TCanvas); override;
    function DrawGraphic(ACanvas: TCanvas; ARect: TRect; ID: string): boolean; override;
    procedure DrawLineBreak(ACanvas: TCanvas; x,y: integer; el: TREElement); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure DoContextForWord(MousePos: TPoint; AWord: string; AElement: TREElement; var Handled: Boolean); virtual;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure DoClickHyperlink(URL, AValue: string); virtual;
    procedure DoClickGraphic(AElement: TREElement); virtual;
    procedure DoCanSelectGraphic(AElement: TREElement; var Allow: boolean); virtual;
    procedure DoCanSizeGraphic(AElement: TREElement; var Allow: boolean); virtual;
    procedure DoPaintEditor; override;
    procedure DoPasteText(var AText: string; var Allow: boolean); virtual;
    procedure DoPasteFormattedText(AStream: TStream); virtual;
    function GetVersionString:string; virtual;

    procedure CaretTimer(Sender: TObject);
    function IsEmoticon(const EmoticonID: string): boolean; override;
    procedure ParseHTML(HTMLValue: string;
      const Images: TCustomImageList = nil; const Pictures: TGDIPPictureContainer = nil);
    procedure CopyToClipboard(s: string); override;
    procedure CopyPictureToClipboard(APicture: TGDIPPicture); override;
    procedure InitializePictureSize(NamedPictureElement: TNamedPictureElement); override;
    function HintAtXY(X,Y: integer): string;

    procedure Refresh; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Resize; override;
    procedure Paint; override;
    procedure InitContextMenu; virtual;
    procedure InitVCLStyle(init: boolean);
    procedure BeginPaintBuffer; override;
    procedure EndPaintBuffer; override;
    function CreateBuffer: Graphics.TBitmap; override;
    procedure UpdateSelection; override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    function SetRange(ARange: TSize): boolean; override;
    procedure SetRichEditorPopup(AValue: TAdvRichEditorPopup);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Print;
    property ZoomFactor: double read FZoomFactor write SetZoomFactor;
    property SingleLine: boolean read FSingleLine write FSingleLine;

    procedure CopyFormattedSelectionToClipboard;
    procedure CopyRTFToClipboard;
    procedure CopyHTMLToClipboard;
    procedure PasteFormattedSelectionFromClipboard;
    function PasteFromClipboard: string; override;
    function ClipboardHasContent: boolean; override;
    property TabSize;
    property ClipboardFormats: TClipboardFormats read FClipboardFormats write FClipboardFormats;
    property BulletSpacing;
  published
    property Align;
    property AllowSelect: boolean read FAllowSelect write FAllowSelect default True;
    property Anchors;
    property AutoCorrect;
    property BorderStyle;
    property Color default clWhite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property GraphicSelection: TGraphicSelection read FGraphicSelection write SetGraphicSelection;
    property Font;
    property ParentFont;
  {$IFDEF TMSPACK}
    property Emoticons: TGDIPPictureContainer read FEmoticons write FEmoticons;
    property PictureContainer: TGDIPPictureContainer read FGDIPPictureContainer write FGDIPPictureContainer;
  {$ENDIF}
    property PopupMenu;
    property ReadOnly;
    property PopupToolBar: TAdvRichEditorPopup read FRichEditorPopup write SetRichEditorPopup;
    property ShowHint;
    property TabOrder;
    property TabStop;
  {$IFNDEF LCLLIB}
    property Touch;
  {$ENDIF}
    property Version: string read GetVersion write SetVersion;
    property Visible;
    property WantTab default false;

    property OnCanSelectGraphic: TGraphicAllowEvent read FOnCanSelectGraphic write FOnCanSelectGraphic;
    property OnCanSizeGraphic: TGraphicAllowEvent read FOnCanSizeGraphic write FOnCanSizeGraphic;
    property OnCaretChanged;
    property OnClick;
    property OnClickGraphic: TGraphicClickEvent read FOnClickGraphic write FOnClickGraphic;
    property OnClickHyperlink: THyperlinkClickEvent read FOnClickHyperlink write FOnClickHyperlink;

    property OnContextPopup;
    property OnContextForWord: TWordContextEvent read FOnContextForWord write FOnContextForWord;
    property OnCorrectWord;
    property OnDblClick;
    property OnDrawGraphic: TRichEditorDrawGraphicEvent read FOnDrawGraphic write FOnDrawGraphic;
    property OnDragOver;
    property OnDragDrop;
    property OnEnter;
    property OnEnterWord;
    property OnExit;
    property OnEndDock;
    property OnEndDrag;
  {$IFNDEF LCLLIB}
    property OnGesture;
    property OnMouseActivate;
  {$ENDIF}
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnPasteText: TPasteTextEvent read FOnPasteText write FOnPasteText;
    property OnPasteFormattedText: TPasteFormattedTextEvent read FOnPasteFormattedText write FOnPasteFormattedText;
    property OnSelectionChanged;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  SysUtils, Math, ShellAPI, CommCtrl, ActiveX, StrUtils
{$IFDEF DELPHIXE2_LVL}
  , VCL.Themes
{$ENDIF}
  ;

{$IFDEF FREEWARE}
const
  trialversion = ' trial version ';
{$ENDIF}

var
  HTMLEncodedChar : array[0..62] of string = ('&','<','>','"',' ',
                                              'é','è','ë','ê',
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
                                              '€','«','»','ã',
                                              'Ã','õ','Õ','™',
                                              '§','¶');


procedure TAdvRichEditor.BeginPaintBuffer;
begin
  Buffer.Width := GetClientWidth;
  Buffer.Height := GetClientHeight;
end;

procedure TAdvRichEditor.CaretTimer(Sender: TObject);
begin
  FDoCaret := not FDoCaret;
  FCaretUpdate := true;
  Paint;
  FCaretUpdate := false;
end;


function TAdvRichEditor.HintAtXY(X,Y: integer): string;
var
  el: TREElement;
  SP: TPoint;
begin
  Result := Hint;

  SP := TopLeft;

  XYToElement(X + SP.X, Y + SP.Y,el);

  if Assigned(el) and (el.Hint <> '') then
    Result := el.Hint;
end;

function TAdvRichEditor.ClipboardHasContent: boolean;
var
  Format: word;
begin
  Result := false;

  OpenClipboard(Handle);

  Format := EnumClipboardFormats(0);
  while Format <> 0 do
  begin
    if ClipboardHasText(Format) then
      Result := True;
    if ClipboardHasBitmap(Format) then
      Result := True;
    if ClipboardHasPicture(Format) then
      Result := True;
    if ClipboardHasRTE(Format) then
      Result := True;

    if Result then
      break;
    Format := EnumClipboardFormats(Format);
  end;

  CloseClipboard;
end;

procedure TAdvRichEditor.CMHintShow(var Msg: TMessage);
var
  PHI: PHintInfo;
begin
  PHI := TCMHintShow(Msg).HintInfo;

  PHI^.HintStr := HintAtXY(PHI^.CursorPos.X, PHI^.CursorPos.Y);

  FLastHint := PHI^.HintStr;
end;

procedure TAdvRichEditor.CMColorChanged(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) and not (csReading in ComponentState) then
    DefaultChanged := true;
end;

{$IFDEF DELPHIXE2_LVL}
procedure TAdvRichEditor.CMStyleChanged(var Message: TMessage);
begin
  InitVCLStyle(true);
end;
{$ENDIF}

procedure TAdvRichEditor.CMShowingChanged(var Message: TMessage);
begin
  inherited;

  if not Visible and Assigned(PopupToolBar) and PopupToolBar.Visible then
    PopupToolBar.Hide;
end;

procedure TAdvRichEditor.CMDialogChar(var Message: TCMDialogChar);
begin
  inherited;
end;

procedure TAdvRichEditor.CMWantSpecialKey(var Msg:TCMWantSpecialKey);
begin
  inherited;
  if (Msg.CharCode = VK_RETURN) then
    Msg.Result := 1;
end;

procedure TAdvRichEditor.PasteFormattedSelectionFromClipboard;
var
  DataPtr: Pointer;
  Data: THandle;
  AStream: TMemoryStream;
  ms: TMemoState;
  mss: TStateSaver;
  f: double;
begin
  if Clipboard.HasFormat(ClipboardRTEFMT) then
  begin
    // this is the preferred format ??
    Clipboard.Open;

    try
      Data := GetClipboardData(ClipboardRTEFMT);
      if Data = 0 then
      begin
        Clipboard.Close;
        Exit;
      end;

      DataPtr := GlobalLock(Data);
      if DataPtr = nil then
      begin
        Clipboard.Close;
        Exit;
      end;

      AStream := TMemoryStream.Create;
      try
        AStream.WriteBuffer(DataPtr^, GlobalSize(Data));
        AStream.Position := 0;

        if HasSelection then
          DeleteSelection;

        mss := TStateSaver.Create(nil);
        ms := TMemoState.Create;
        mss.SaveState := ms;
        try
          AStream.ReadComponent(mss);
          LoadMemoState(ms, True);
        finally
          mss.Free;
          ms.Free;
        end;

        f := 1;

        if (Producer = pFMX) then
          f := 72/96;

        DoPasteFormattedText(AStream);

        InsertFromStream(AStream, f);
      finally
        AStream.Free;
      end;
    finally
      Clipboard.Close;
    end;
  end;
end;

procedure TAdvRichEditor.CopyFormattedSelectionToClipboard;
var
  ms: TMemoryStream;
  Data: THandle;
  DataPtr: Pointer;
begin
  ms := TMemoryStream.Create;

  Clipboard.Open;

  try
    SaveSelectionToStream(ms);
    ms.Position := 0;

    Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, ms.Size);
    try
      DataPtr := GlobalLock(Data);
      try
        Move(ms.Memory^, DataPtr^, ms.Size);
        SetClipboardData(ClipboardRTEFMT, Data);
      finally
        GlobalUnlock(Data);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    ms.Free;
    Clipboard.Close;
  end;
end;


procedure TAdvRichEditor.CopyHTMLToClipboard;
var
  htmlstr: ansistring;
  MemHandleHTML: THandle;

begin
  htmlstr := AnsiString(GetContentAsHTML(False));

  // standard HTML clipboard format preablme
  htmlstr := AnsiString('Version:0.9'#13#10
    +'StartHTML:71'#13#10
    +'EndHTML:'+inttostr(length(htmlstr) + 71)+#13#10
    +'StartFragment:140'#13#10
    +'EndFragment:'+inttostr(length(htmlstr)+ 142)+#13#10
    +'<!DOCTYPE>' +#13#10
    +'<HTML>'#13#10
    +'<HEAD>'#13#10
    +'<TITLE>The HTML Clipboard</TITLE>'#13#10
    +'<BASE HREF="http://sample/specs">'#13#10
    +'</HEAD>'#13#10
    +'<BODY>'#13#10
    +'<!--StartFragment -->'#13#10)
    + htmlstr + AnsiString(#13#10
    +'<!--StartFragment -->'#13#10
    +'</BODY>'#13#10
    +'</HTML>'#13#10);

  MemHandleHTML := GlobalAlloc(GHND or GMEM_SHARE, Length(htmlstr) + 1);

  if (MemHandleHTML <> 0) then
  begin
    {$IFDEF DELPHIXE4_LVL}
    AnsiStrings.StrCopy(GlobalLock(MemHandleHTML), PAnsiChar(htmlstr));
    {$ENDIF}
    {$IFNDEF DELPHIXE4_LVL}
    StrCopy(GlobalLock(MemHandleHTML), PAnsiChar(htmlstr));
    {$ENDIF}
    GlobalUnlock(MemHandleHTML);
    {$IFNDEF LCLLIB}
    Clipboard.SetAsHandle(ClipboardHTMLFMT, MemHandleHTML);
    {$ENDIF}
  end;
end;

procedure TAdvRichEditor.CopyToClipboard(s: string);
begin
  if Context.Content.Count > 0 then
  begin
    Clipboard.Open;

    Clipboard.AsText := s;

    CopyFormattedSelectionToClipboard;

    CopyRTFToClipboard;

    Clipboard.Close;
  end;
end;

procedure TAdvRichEditor.CopyPictureToClipboard(APicture: TGDIPPicture);
var
  pic: TGraphic;
  ms: TMemoryStream;
begin
  pic := nil;

  {$IFNDEF LCLLIB}
  case APicture.PictureFormat of
  pfBMP: pic := Graphics.TBitmap.Create;
  pfJPG: pic := TJPEGImage.Create;
  pfPNG: pic := TPngImage.Create;
  pfGIF: pic := TGIFImage.Create;
  pfICO: pic := TIcon.Create;
  end;
  {$ENDIF}

  if Assigned(pic) then
  begin
    try
      ms := TMemoryStream.Create;
      try
        APicture.SaveToStream(ms);
        ms.Position := 0;
        pic.LoadFromStream(ms);
      finally
        ms.Free;
      end;

      Clipboard.Open;
      Clipboard.Assign(pic);
      Clipboard.Close;

    finally
      pic.Free;
    end;
  end;
end;


procedure TAdvRichEditor.CopyRTFToClipboard;
var
  rtfstr: ansistring;
  MemHandleRTF: THandle;

begin
  rtfstr := ansistring(GetContentAsRTF(true));

  MemHandleRTF := GlobalAlloc(GHND or GMEM_SHARE, Length(rtfstr) + 1);

  if (MemHandleRTF <> 0) then
  begin
    {$IFDEF DELPHIXE4_LVL}
    AnsiStrings.StrCopy(GlobalLock(MemHandleRTF), PAnsiChar(rtfstr));
    {$ENDIF}
    {$IFNDEF DELPHIXE4_LVL}
    StrCopy(GlobalLock(MemHandleRTF), PAnsiChar(rtfstr));
    {$ENDIF}

    GlobalUnlock(MemHandleRTF);
    {$IFNDEF LCLLIB}
    Clipboard.SetAsHandle(ClipboardRTFFMT, MemHandleRTF);
    {$ENDIF}
  end;
end;

constructor TAdvRichEditor.Create(AOwner: TComponent);
begin
  inherited;
  Cursor := crIBeam;
  DoubleBuffered := true;
  Color := clWhite;
  Width := 400;
  Height := 300;
  FZoomFactor := 1.0;
  TabStop := true;
  FInternalDD := false;
  FCaretTimer := TTimer.Create(Self);
  FCaretTimer.Interval := 500;
  FCaretTimer.OnTimer := CaretTimer;
  FPopupMenu := TPopupMenu.Create(Self);
  FImageList := TImageList.Create(Self);

  FCutAction := TAdvRichEditorCut.Create(Self);
  FCutAction.Caption := 'Cut';
  FCutAction.ImageIndex := 0;

  FCopyAction := TAdvRichEditorCopy.Create(Self);
  FCopyAction.Caption := 'Copy';
  FCopyAction.ImageIndex := 1;

  FPasteAction := TAdvRichEditorPaste.Create(Self);
  FPasteAction.Caption := 'Paste';
  FPasteAction.ImageIndex := 2;

  FLeftAction := TAdvRichEditorAlignLeft.Create(Self);
  FLeftAction.Caption := 'Left';

  FCenterAction := TAdvRichEditorAlignCenter.Create(Self);
  FCenterAction.Caption := 'Center';

  FRightAction := TAdvRichEditorAlignRight.Create(Self);
  FRightAction.Caption := 'Right';

  FClearAction := TAdvRichEditorClear.Create(Self);
  FClearAction.Caption := 'Clear';
  FClearAction.ImageIndex := -1;

  FGraphicSelection := TGraphicSelection.Create;
  FSingleLine := False;
  FAllowSelect := True;

{$IFDEF TMSPACK}
  FGDIPPicture := TGDIPPicture.Create;
{$ENDIF}

  FClipboardFormats := [cfRTE, cfRTF, cfText, cfHTML, cfFile, cfBMP];
  InitContextMenu;
end;

function TAdvRichEditor.CreateBuffer: Graphics.TBitmap;
begin
  Result := Graphics.TBitmap.Create;
end;

procedure TAdvRichEditor.CreateWnd;
begin
  inherited;
  {$IFNDEF LCLLIB}
  FDropTarget := TAdvRichEditorDropTarget.Create(Self);
  FDropTargetAssigned := RegisterDragDrop(Handle, FDropTarget) = S_OK;

  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    InitVCLStyle(false);
  {$ENDIF}
end;

destructor TAdvRichEditor.Destroy;
begin
  FGraphicSelection.Free;
  FPopupMenu.Free;
  FCutAction.Free;
  FCopyAction.Free;
  FPasteAction.Free;
  FClearAction.Free;
  FImageList.Free;
  FCaretTimer.Free;
{$IFDEF TMSPACK}
  FGDIPPicture.Free;
{$ENDIF}
  inherited;
end;

procedure TAdvRichEditor.DestroyWnd;
begin
  inherited;
end;

procedure TAdvRichEditor.Loaded;
begin
  inherited;
  DefaultChanged := false;
  InitVCLStyle(false);
end;

procedure TAdvRichEditor.DoClickGraphic(AElement: TREElement);
begin
  if Assigned(OnClickGraphic) then
    OnClickGraphic(Self, AElement);
end;

procedure TAdvRichEditor.DoCanSelectGraphic(AElement: TREElement; var Allow: boolean);
begin
  if Assigned(OnCanSelectGraphic) then
    OnCanSelectGraphic(Self, AElement, Allow);
end;

procedure TAdvRichEditor.DoCanSizeGraphic(AElement: TREElement; var Allow: boolean);
begin
  if Assigned(OnCanSizeGraphic) then
    OnCanSizeGraphic(Self, AElement, Allow);
end;

procedure TAdvRichEditor.DoClickHyperlink(URL, AValue: string);
begin
  if Assigned(OnClickHyperlink) then
    OnClickHyperlink(Self, URL)
  else
  begin
    if (URL = '#') then
      URL := AValue;

    ShellExecute(0,'open',PChar(URL),nil,nil,SW_NORMAL);
  end;
end;

procedure TAdvRichEditor.DoEnter;
begin
  inherited;

  if not Assigned(Caret.Element) or (Caret.CharIndex = -1) then
  begin
    Caret.Element := nil;
    Caret.CharIndex := 0;
  end;

  Refresh;
end;

procedure TAdvRichEditor.DoExit;
begin
  inherited;
  Refresh;
end;

function StrPScan(s: string; ch: char; FromPos: integer): integer;
var
  i: integer;
begin
  Result := -1;

  for i := FromPos to Length(s) do
  begin
    if CharInStr(s, i) = ch then
    begin
      Result := i;
      break;
    end;
  end;
end;

function StripQuotes(s: string): string;
var
  ch: char;
begin
  if Length(s) >= 2 then
  begin
    ch := CharInStr(s,1);

    if (ch = '"') or (ch = '''') then
    begin
      Delete(s,1,1);
    end;

    ch := CharInStr(s, Length(s));

    if (ch = '"') or (ch = '''') then
    begin
      Delete(s,Length(s),1);
    end;
  end;

  Result := s;
end;


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

function Hex2Color(s: string): TColor;
var
  r,g,b: Integer;
begin
  r := Hexval(Copy(s,1,2));
  g := Hexval(Copy(s,3,2)) shl 8;
  b := Hexval(Copy(s,5,2)) shl 16;
  Result := TColor(b + g + r);
end;

function HTMLColor(s: string): TColor;
begin
  // official HTML color names lookup

  if pos('CL',s) = 1 then
    Delete(s,1,2);

  Result := clNone;
  case IndexText(s, ['INDIANRED', 'LIGHTCORAL', 'SALMON', 'DARKSALMON', 'LIGHTSALMON', 'CRIMSON', 'RED', 'FIREBRICK',
                     'DARKRED', 'PINK', 'LIGHTPINK', 'HOTPINK', 'DEEPPINK', 'MEDIUMVIOLETRED', 'PALEVIOLETRED', 'LIGHTSALMON',
                     'CORAL', 'TOMATO', 'ORANGERED', 'DARKORANGE', 'ORANGE', 'GOLD', 'YELLOW', 'LIGHTYELLOW', 'LEMONCHIFFON',
                     'LIGHTGOLDENRODYELLOW', 'PAPAYAWHIP', 'MOCCASIN', 'PEACHPUFF', 'PALEGOLDENROD', 'KHAKI', 'DARKKHAKI', 'LAVENDER',
                     'THISTLE', 'PLUM', 'VIOLET', 'ORCHID', 'FUCHSIA', 'MAGENTA', 'MEDIUMORCHID', 'MEDIUMPURPLE', 'AMETHYST',
                     'BLUEVIOLET', 'DARKVIOLET', 'DARKORCHID', 'DARKMAGENTA', 'PURPLE', 'INDIGO', 'SLATEBLUE', 'DARKSLATEBLUE',
                     'MEDIUMSLATEBLUE', 'GREENYELLOW', 'CHARTREUSE', 'LAWNGREEN', 'LIME', 'LIMEGREEN', 'PALEGREEN', 'LIGHTGREEN',
                     'MEDIUMSPRINGGREEN', 'SPRINGGREEN', 'MEDIUMSEAGREEN', 'SEAGREEN', 'FORESTGREEN', 'GREEN', 'DARKGREEN',
                     'YELLOWGREEN', 'OLIVEDRAB', 'OLIVE', 'DARKOLIVEGREEN', 'MEDIUMAQUAMARINE', 'DARKSEAGREEN', 'LIGHTSEAGREEN',
                     'DARKCYAN', 'TEAL', 'AQUA', 'CYAN', 'LIGHTCYAN', 'PALETURQUOISE', 'AQUA', 'ARINE', 'TURQUOISE', 'MEDIUMTURQUOISE',
                     'DARKTURQUOISE', 'CADETBLUE', 'STEELBLUE', 'LIGHTSTEELBLUE', 'POWDERBLUE', 'LIGHTBLUE', 'SKYBLUE', 'LIGHTSKYBLUE',
                     'DEEPSKYBLUE', 'DODGERBLUE', 'CORNFLOWERBLUE', 'MEDIUMSLATEBLUE', 'ROYALBLUE', 'BLUE', 'MEDIUMBLUE', 'DARKBLUE',
                     'NAVY', 'MIDNIGHTBLUE', 'CORNSILK', 'BLANCHEDALMOND', 'BISQUE', 'NAVAJOWHITE', 'WHEAT', 'BURLYWOOD', 'TAN',
                     'ROSYBROWN', 'SANDYBROWN', 'GOLDENROD', 'DARKGOLDENROD', 'PERU', 'CHOCOLATE', 'SADDLEBROWN', 'SIENNA', 'BROWN',
                     'MAROON', 'WHITE', 'SNOW', 'HONEYDEW', 'MINTCREAM', 'AZURE', 'ALICEBLUE', 'GHOSTWHITE', 'WHITESMOKE', 'SEASHELL',
                     'BEIGE', 'OLDLACE', 'FLORALWHITE', 'IVORY', 'ANTIQUEWHITE', 'LINEN', 'LAVENDERBLUSH', 'MISTYROSE', 'GAINSBORO',
                     'LIGHTGREY', 'SILVER', 'DARKGRAY', 'GRAY', 'DIMGRAY', 'LIGHTSLATEGRAY', 'SLATEGRAY', 'DARKSLATEGRAY', 'BLACK']) of
    0: Result := $5C5CCD;
    1: Result := $8080F0;
    2: Result := $7280FA;
    3: Result := $7A96E9;
    4: Result := $7AA0FF;
    5: Result := $3C14DC;
    6: Result := $0000FF;
    7: Result := $2222B2;
    8: Result := $00008B;
    9: Result := $CBC0FF;
    10: Result := $C1B6FF;
    11: Result := $B469FF;
    12: Result := $9314FF;
    13: Result := $8515C7;
    14: Result := $9370DB;
    15: Result := $7AA0FF;
    16: Result := $507FFF;
    17: Result := $4763FF;
    18: Result := $0045FF;
    19: Result := $008CFF;
    20: Result := $00A5FF;
    21: Result := $00D7FF;
    22: Result := $00FFFF;
    23: Result := $E0FFFF;
    24: Result := $CDFAFF;
    25: Result := $D2FAFA;
    26: Result := $D5EFFF;
    27: Result := $B5E4FF;
    28: Result := $B9DAFF;
    29: Result := $AAE8EE;
    30: Result := $8CE6F0;
    31: Result := $6BB7BD;
    32: Result := $FAE6E6;
    33: Result := $D8BFD8;
    34: Result := $DDA0DD;
    35: Result := $EE82EE;
    36: Result := $D670DA;
    37: Result := $FF00FF;
    38: Result := $FF00FF;
    39: Result := $D355BA;
    40: Result := $DB7093;
    41: Result := $CC6699;
    42: Result := $E22B8A;
    43: Result := $D30094;
    44: Result := $CC3299;
    45: Result := $8B008B;
    46: Result := $800080;
    47: Result := $82004B;
    48: Result := $CD5A6A;
    49: Result := $8B3D48;
    50: Result := $EE687B;
    51: Result := $2FFFAD;
    52: Result := $00FF7F;
    53: Result := $00FC7C;
    54: Result := $00FF00;
    55: Result := $32CD32;
    56: Result := $98FB98;
    57: Result := $90EE90;
    58: Result := $9AFA00;
    59: Result := $7FFF00;
    60: Result := $71B33C;
    61: Result := $578B2E;
    62: Result := $228B22;
    63: Result := $008000;
    64: Result := $006400;
    65: Result := $32CD9A;
    66: Result := $238E6B;
    67: Result := $008080;
    68: Result := $2F6B55;
    69: Result := $AACD66;
    70: Result := $8FBC8F;
    71: Result := $AAB220;
    72: Result := $8B8B00;
    73: Result := $808000;
    74: Result := $FFFF00;
    75: Result := $FFFF00;
    76: Result := $FFFFE0;
    77: Result := $EEEEAF;
    78: Result := $D4FF7F;
    79: Result := $D0E040;
    80: Result := $CCD148;
    81: Result := $D1CE00;
    82: Result := $A09E5F;
    83: Result := $B48246;
    84: Result := $DEC4B0;
    85: Result := $E6E0B0;
    86: Result := $E6D8AD;
    87: Result := $EBCE87;
    88: Result := $FACE87;
    89: Result := $FFBF00;
    90: Result := $FF901E;
    91: Result := $ED9564;
    92: Result := $EE687B;
    93: Result := $E16941;
    94: Result := $FF0000;
    95: Result := $CD0000;
    96: Result := $8B0000;
    97: Result := $800000;
    98: Result := $701919;
    99: Result := $DCF8FF;
    100: Result := $CDEBFF;
    101: Result := $C4E4FF;
    102: Result := $ADDEFF;
    103: Result := $B3DEF5;
    104: Result := $87B8DE;
    105: Result := $8CB4D2;
    106: Result := $8F8FBC;
    107: Result := $60A4F4;
    108: Result := $20A5DA;
    109: Result := $0B86B8;
    110: Result := $3F85CD;
    111: Result := $1E69D2;
    112: Result := $13458B;
    113: Result := $2D52A0;
    114: Result := $2A2AA5;
    115: Result := $000080;
    116: Result := $FFFFFF;
    117: Result := $FAFAFF;
    118: Result := $F0FFF0;
    119: Result := $FAFFF5;
    120: Result := $FFFFF0;
    121: Result := $FFF8F0;
    122: Result := $FFF8F8;
    123: Result := $F5F5F5;
    124: Result := $EEF5FF;
    125: Result := $DCF5F5;
    126: Result := $E6F5FD;
    127: Result := $F0FAFF;
    128: Result := $F0FFFF;
    129: Result := $D7EBFA;
    130: Result := $E6F0FA;
    131: Result := $F5F0FF;
    132: Result := $E1E4FF;
    133: Result := $DCDCDC;
    134: Result := $D3D3D3;
    135: Result := $C0C0C0;
    136: Result := $A9A9A9;
    137: Result := $808080;
    138: Result := $696969;
    139: Result := $998877;
    140: Result := $908070;
    141: Result := $4F4F2F;
    142: Result := $000000;
  end;
end;

function HTML2Color(s: string): TColor;
begin
  if Pos('#',s) = 1 then
  begin
    Delete(s,1,1);
    Result := Hex2Color(s);
  end
  else
    Result := HTMLColor(s);
end;

procedure TAdvRichEditor.ParseHTML(HTMLValue: string;
  const Images: TCustomImageList; const Pictures: TGDIPPictureContainer);
var
  TagName,TagP, sIdx,clr,fface,fsize,aclass: string;
  TagParams: TStringList;
  fSkipInitWhitespace, fPar, fSpace: Boolean;
  fBullet: TBulletType;
  fParAlign: TAlignment;
  mParAlign: TAlignment;
  {$IFDEF TMSPACK}
  APicture: TGraphic;
  {$ENDIF}
  LastColor,LastBkColor: TColor;
  BulletIndex: integer;
  bs: TBulletStart;
  be: TBulletEnd;
  bu: TBulletElement;
  PP,PT,PP2,FSI: integer;
  alignchange: boolean;
  hypchange: boolean;
  appleclass: boolean;
  isDoc, isXML: boolean;
  hypsel: TREElement;
  hypselidx,specialidx: integer;
  hypurl,specialstr: string;


begin
  // supports these tags:
  // B, I, U, S, A, P, BR, IMG, STRONG, EM, SUP, SUB, UL, OL, IL
  // &amp; &lt; &gt; &quot;   "

  if (HtmlValue = '') then
    Exit;

  //Clear;

  FBullet := GetSelectionBullet;
  FSkipInitWhitespace := True;       // skip initial white spaces
  FSpace := False;            // insert a single space
  FPar := False;              // insert a new paragraph
  alignchange := false;
  mParAlign := taLeftJustify;
  hypurl := '';
  hypsel := nil;
  hypselidx := 0;
  hypchange := false;
  appleclass := false;
  isDoc := false;
  isXML := false;

  //if IsSelectionCenter then
  //  FParAlign := taCenter
  //else
  //  if IsSelectionRight then
  //    FParAlign := taRightJustify
  //  else
      FParAlign := taLeftJustify;


  LastColor := GetDefaultFontColor;
  LastBkColor := Color;

  SelectionType := stSingle;

  TagParams := nil;
  BulletIndex := 0;

  BeginUpdate;

  try
    TagParams := TStringList.Create;

    PP := 1;

    while (PP <= Length(HtmlValue)) do
    begin
      if (CharInStr(HtmlValue, PP) = '<') then
      begin
         // handle a tag
         Inc(PP);

         PT := PP;
         while not CharInSet(CharInStr(HtmlValue,PP), ['>', ' ', #0, '[',#10,#13]) do
           Inc(PP);

         TagName := Copy(HtmlValue, PT, PP - PT);
         TagParams.Clear;

         while (not CharInSet(CharInStr(HtmlValue, PP), ['>', #0])) do
         begin
           while CharInSet(CharInStr(HtmlValue, PP), [' ', '/']) do
             Inc(PP);

           PT := PP;
           while not CharInSet(CharInStr(HtmlValue,PP), ['>', #0, '"', '''', ' ']) do
           begin
             Inc(PP);
             if CharInSet(CharInStr(HtmlValue,PP), ['"', '''']) then
             begin
               PP2 := StrPScan(HtmlValue, CharInStr(HtmlValue, PP), PP + 1);

               if PP2 <> -1 then
                 PP := PP2;

               Inc(PP);
             end;
           end;
           if (PP <> PT) then
           begin
             TagP := Copy(HtmlValue, PT, PP - PT);
             TagParams.Add(TagP);
           end;
         end;

         if (CharInStr(HtmlValue,PP) = '>') then
         begin
           Inc(PP);
           if isDoc then
             isDoc := false;
         end
         else
           break; //unclosed left parenthesis

         case IndexText(TagName, ['B', '/B', 'STRONG', '/STRONG', 'I', '/I',
                                  'EM', '/EM', 'U', '/U', 'P', '/P', 'BR',
                                  'IMG', 'S', '/S', 'SUP', '/SUP', 'SUB',
                                  '/SUB', 'UL', 'OL', '/UL', '/OL', 'LI', '/LI','FONT','/FONT','SPAN','/SPAN','A','/A','XML','/XML','!--','STYLE','/STYLE','BODY','/BODY']) of
           0, 2: SetSelectionBold(True);
           1, 3: SetSelectionBold(False);
           4, 6: SetSelectionItalic(True);
           5, 7: SetSelectionItalic(False);
           8: SetSelectionUnderline(True);
           9: SetSelectionUnderline(False);
           10:
             begin                 { <P> }
               fSpace := False;
               fBullet := btNone;
               mParAlign := taLeftJustify;
               case IndexText(StripQuotes(TagParams.Values['ALIGN']), ['center', '"center"','right', '"right"']) of
               0, 1: mParAlign := taCenter;
               2, 3: mParAlign := taRightJustify;
               end;
               if not fSkipInitWhitespace or (fParAlign <> mParAlign) or fPar then
               begin
                 alignchange := true;
                 if not (LastElement is TLineBreakElement) then AppendLineBreak;
                 //SetSelectionAttribute(mParAlign);
                 fParAlign := mParAlign;
                 //fSkipInitWhitespace := True;
                 fPar := False;
                 fSpace := False;
               end;

               clr := StripQuotes(TagParams.Values['BGCOLOR']);
               if (clr <> '') then
               begin
                 SetSelectionBkColor(HTML2Color(Uppercase(clr)));
               end;

             end;
           11:
             begin
               fPar := True;       { </P> }
               fSpace := False;
               if mParAlign <> taLeftJustify then
               begin
                 mParAlign := taLeftJustify;
                 alignchange := true;
//                 SetSelectionAttribute(mParAlign);
               end;
               fSkipInitWhitespace := True;
             end;
           12:
             begin
               AppendLineBreak; { <BR> }
               SetSelectionAttribute(fParAlign);
               fPar := False;
               fSpace := False;
               fSkipInitWhitespace := True;
             end;
           13:
             begin                  { <IMG> }
               sIdx := StripQuotes(TagParams.Values['SRC']);

               if pos('file://', sIdx) = 1 then
               begin
                 Delete(sIdx,1,7);
                 if FileExists(sIdx) then
                 begin
                   InsertImage(sIdx);
                   SetCaret(cpEndDoc);
                 end;
               end;

               if (Length(sIdx) >= 2) and (CharInStr(sIdx, 1) = CharInStr(sIdx,Length(sIdx))) and CharInSet(CharInStr(sIdx,1), ['''', '"']) then
                 sIdx := AnsiDequotedStr(sIdx, CharInStr(sIdx,1));
               if (fPar) then
               begin
                 AppendLineBreak;
                 SetSelectionAttribute(fParAlign);
                 fPar := False;
               end
               else if (FSpace) then
                 InsertText(' ');
               fSpace := False;
               if Assigned(Images) and (Pos('IDX:',Uppercase(sIdx)) = 1) then
               begin
                 InsertGraphic(sIdx, Images.Width + 4, Images.Height + 4);
                 SetSelectionFontSize(Font.Size);
               end
               else if Assigned(Pictures) then
               begin
                 {$IFDEF TMSPACK}
                 APicture := Pictures.FindPicture(sIdx);
                 if Assigned(APicture) then
                 begin
                   InsertGraphic(sIdx, APicture.Width + 4, APicture.Height + 4);
                   SetSelectionFontSize(Font.Size);
                 end;
                 {$ENDIF}
               end;
             end;
           14: SetSelectionStrikeOut(True);
           15: SetSelectionStrikeOut(False);
           16: SetSelectionSuperscript(True);
           17: SetSelectionSuperscript(False);
           18: SetSelectionSubscript(True);
           19: SetSelectionSubscript(False);
           20:
             begin
               AppendLineBreak;
               fBullet := btCircle; // UL

               bs := TBulletStart.Create;
               bs.Assign(LastElement);
               bs.Indent := 24;
               BulletIndex := 0;
               Context.Content.Add(bs);
             end;
           21:
             begin
               AppendLineBreak;
               fBullet := btNumber; // OL
               BulletIndex := 0;
             end;
           22, 23:
             begin // /UL, /OL
               fBullet := btNone;
               fPar := True;
               fSkipInitWhitespace := True;

               be := TBulletEnd.Create;
               be.Assign(LastElement);

               Context.Content.Add(be);
               AppendLineBreak;
             end;
           24: // LI
             begin
               SetCaret(cpEndDoc);
               //TAdvRichEditorEx(RichEditor).InsertLineBreakAndBullet(fBullet, 24);

               if BulletIndex > 0 then
                 AppendLineBreak;

               bu := TBulletElement.Create;
               bu.Assign(LastElement);
               bu.&Type := fBullet;
               bu.Indent := 24;
               bu.Index := BulletIndex;
               bu.BulletFont := LastElement.Font.Name;

               inc(BulletIndex);

               //SetCaret(cpEndDoc);
               //Caret.CharIndex := 0;

               Context.Content.Add(bu);
               Caret.Element := bu;
               Caret.CharIndex := 1;

               SetSelectionAttribute(fParAlign);
               fSpace := False;
               fPar := False;
               fSkipInitWhitespace := True;
             end;
           25:
             begin // /LI
               fPar := True;
               fSpace := False;
               fSkipInitWhitespace := True;
             end;
           26: //  FONT
             begin
               LastColor := GetSelectionTextColor;
               clr := StripQuotes(TagParams.Values['COLOR']);
               if (clr <> '') then
               begin
                 SetSelectionColor(HTML2Color(Uppercase(clr)));
               end;
               clr := StripQuotes(TagParams.Values['BGCOLOR']);
               if (clr <> '') then
               begin
                 SetSelectionBkColor(HTML2Color(Uppercase(clr)));
               end;
               fface := StripQuotes(TagParams.Values['FACE']);
               if (fface <> '') then
               begin
                 SetSelectionFontName(fface);
               end;

               fsize := StripQuotes(TagParams.Values['STYLE']);
               if (fsize <> '') then
               begin
                 fsi := Pos('FONT-SIZE:',Uppercase(fsize));

                 if (fsi > 0) then
                 begin
                   fsize := Trim(Copy(fsize,fsi + 10, Length(fsize)));

                   fsi := 1;
                   while CharInSet(CharInStr(fsize,fsi),['0'..'9']) do
                     inc(fsi);

                   fsize := Copy(fsize,1,fsi - 1);

                   SetSelectionFontSize(strtoint(fsize));
                 end;
              end;

             end;
           27: //  /FONT
             begin
               SetSelectionColor(LastColor);
             end;
           28:
             begin
               LastBkColor := GetSelectionBkColor;
               clr := Uppercase(TagParams.Values['STYLE']);
               aclass := TagParams.Values['CLASS'];
               if (aclass <> '') then
               begin
                 appleclass := pos('APPLE-', Uppercase(aclass)) > 0;
               end;

               if (clr <> '') then
               begin
                 //style="background-color:#FFFF00">

                 if pos('"BACKGROUND-COLOR:#',clr) = 1 then
                 begin
                   Delete(clr,1,Length('"BACKGROUND-COLOR:'));
                   Delete(clr,Length(clr),1);
                   SetSelectionBkColor(HTML2Color(clr));
                 end;

                 if pos('COLOR:',clr) = 2 then
                 begin
                   Delete(clr,1,1 + Length('COLOR:'));
                   Delete(clr,Length(clr),1);
                   SetSelectionColor(HTML2Color(clr));
                 end;
               end;
             end;
           29:
             begin
               appleclass := false;
               SetSelectionBkColor(LastBkColor);
             end;
           30:   // start hyperlink
             begin
               hypchange := true;
               hypurl := StripQuotes(TagParams.Values['HREF']);
             end;
           31:   // end hyperlink
             begin
               SetCaret(cpEndDoc);

               Selection.FromElement := hypsel;
               Selection.FromChar := hypselidx;
               Selection.ToElement := Caret.Element;
               Selection.ToChar := Caret.CharIndex;

               SetSelectionHyperlink(hypurl);
               SetCaret(cpEndDoc);
               DefaultChanged := true;
             end;
           32: // <XML>
             begin
               isXML := true;
             end;
           33: // </XML>
             begin
               isXML := false;
             end;
           34: // <!--
             begin
               isDoc := true;
             end;
           35:
             begin
               isDoc := true;
             end;
           36:
             begin
               isDoc := false;
             end;
           37:
             begin
               clr := StripQuotes(TagParams.Values['BGCOLOR']);
               if (clr <> '') then
               begin
                 SetSelectionBkColor(HTML2Color(Uppercase(clr)));
               end;

             end;
           38:
             begin

             end;
           // ignore other tags
         end;
       end
       else if (Ord(CharInStr(HtmlValue,PP)) <= 32) then
       begin
         fSpace := not fSkipInitWhitespace;
         Inc(PP);
       end
       else
       if appleclass then
       begin
         while (CharInStr(HtmlValue, PP) <> '<') do Inc(PP);
           InsertChar(' ');
       end
       else
       if (CharInStr(HtmlValue, PP) = '&') then
       begin
         Inc(PP);
         PT := PP;
         while CharInSet(CharInStr(HtmlValue, PP), ['A'..'Z', 'a'..'z']) do Inc(PP);
         if (CharInStr(HtmlValue, PP) = ';') then
           Inc(PP)
         else
           PP := PT;

         if (fPar) then
         begin
           AppendLineBreak;
           SetSelectionAttribute(fParAlign);
           fPar := False;
         end
         else
           if (fSpace) then
             InsertChar(' ');

         fSpace := False;
         fSkipInitWhitespace := False;

         TagName := Copy(HtmlValue, PT, PP - PT);
         specialidx := IndexText(TagName, ['amp;','lt;','gt;','quot;','nbsp;',
                                           'eacute;','egrave;','euml;','ecirc;',
                                           'oacute;','ograve;','ouml;','ocirc;',
                                           'iacute;','igrave;','iuml;','icirc;',
                                           'uacute;','ugrave;','uuml;','ucirc;',
                                           'aacute;','agrave;','auml;','acirc;',
                                           'Eacute;','Egrave;','Euml;','Ecirc;',
                                           'Oacute;','Ograve;','Ouml;','Ocirc;',
                                           'Iacute;','Igrave;','Iuml;','Icirc;',
                                           'Uacute;','Ugrave;','Uuml;','Ucirc;',
                                           'Aacute;','Agrave;','Auml;','Acirc;',
                                           'ccedil;','Ccedil;','oslash;','Oslash;',
                                           'aring;','Aring;','copy;','reg;',
                                           'euro;','laquo;','raquo;','atilde;',
                                           'Atilde;','otilde;','Otilde',
                                           'trade;','sect;','para;']);

         if (specialidx >= 0) then
           specialstr := HTMLEncodedChar[specialidx]
         else
           specialstr := '&';

         if not (isDoc or isXML) then
           InsertChar(CharInstr(specialstr,1));

         if alignchange then
         begin
           SetSelectionAttribute(mParAlign);
           alignchange := false;
         end;

         SetCaret(cpEndDoc);

         if hypchange then
         begin
           hypsel := Caret.Element;
           hypselidx := Caret.CharIndex - 1;
           hypchange := false;
         end;

       end
       else
       begin
         if not (isDoc or isXML) then
         begin

           if (fPar) then
           begin
             AppendLineBreak;
             SetSelectionAttribute(fParAlign);
             fPar := False;
           end
           else if (fSpace) then
             InsertChar(' ');

           fSpace := False;
           fSkipInitWhitespace := False;
           InsertChar(CharInStr(HtmlValue, PP));

           if alignchange then
           begin
             SetSelectionAttribute(mParAlign);
             alignchange := false;
           end;

           SetCaret(cpEndDoc);

           if hypchange then
           begin
             hypsel := Caret.Element;
             hypselidx := Caret.CharIndex - 1;
             hypchange := false;
           end;
         end;

         Inc(PP);
       end;
     end;

     if fPar then
     begin
       AppendLineBreak;
     end
     else
     if fSpace then
     begin
       //InsertChar(' ');
       //SetCaret(cpEndDoc);
     end;

  finally
    TagParams.Free;
    SelectionType := stDefault;
    EndUpdate;
  end;

end;

function TAdvRichEditor.IsEmoticon(const EmoticonID: string): boolean;
{$IFDEF TMSPACK}
var
  i: integer;
{$ENDIF}
begin
  Result := false;
{$IFDEF TMSPACK}
  if not Assigned(Emoticons) then
    Exit;

  if Emoticons.Items.Count = 0 then
    Exit;

  for i := 0 to Emoticons.Items.Count - 1 do
  begin
    if (EmoticonID = Emoticons.Items[i].Name) then
    begin
      Result := true;
      break;
    end;
  end;
{$ENDIF}
end;

procedure TAdvRichEditor.DoContextForWord(MousePos: TPoint; AWord: string; AElement: TREElement; var Handled: Boolean);
begin
  if Assigned(OnContextCorrectWord) then
    OnContextCorrectWord(Self, MousePos, AWord, AElement, Handled);

  if Handled then
    Exit;

  if Assigned(OnContextForWord) then
    OnContextForWord(Self, MousePos, AWord, AElement, Handled);
end;


procedure TAdvRichEditor.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
var
  s: string;
  el: TREElement;

begin
  XYToElement(MousePos.X, MousePos.Y, el);

  if Assigned(el) then
  begin
    s := WordAtXY(MousePos.X, MousePos.Y);
    DoContextForWord(MousePos, s, el, Handled);
  end;

  if Handled then
    Exit;

  if not Assigned(PopupMenu) then
    PopupMenu := FPopupMenu;
  inherited;
end;

function TAdvRichEditor.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  FDoCaret := false;
  FCaretTimer.Enabled := false;

  Result := inherited;

  if FSingleLine then
    Exit;

  if Caret.LH > 0 then
    ScrollDown(Caret.LH)
  else
    ScrollDown(20);

  Refresh;
  FCaretTimer.Enabled := true;
end;

function TAdvRichEditor.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  FDoCaret := false;
  FCaretTimer.Enabled := false;

  Result := inherited;

  if FSingleLine then
    Exit;

  if Caret.LH > 0 then
    ScrollUp(Caret.LH)
  else
    ScrollUp(20);

  Refresh;
  FCaretTimer.Enabled := true;
end;

procedure TAdvRichEditor.DoPaintEditor;
begin
  Paint;
end;

procedure TAdvRichEditor.DoPasteFormattedText(AStream: TStream);
begin
  if Assigned(OnPasteFormattedText) then
    OnPasteFormattedText(Self, AStream);
end;

procedure TAdvRichEditor.DoPasteText(var AText: string; var Allow: boolean);
begin
  if Assigned(OnPasteText) then
    OnPasteText(Self, AText, Allow);
end;

procedure TAdvRichEditor.DrawBackground(ACanvas: TCanvas);
var
  r: TRect;
  sp: TPoint;
begin
  r := ClientRect;
  sp := TopLeft;

  r.Top := r.Top - SP.Y;

  r.Left := r.Left + PageMargin.Horizontal;
  r.Right := r.Right - PageMargin.Horizontal;
  r.Top := r.Top + PageMargin.Vertical;

  ACanvas.Brush.Color := Color;
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Color := ACanvas.Brush.Color;

  ACanvas.Rectangle(r);
end;

procedure TAdvRichEditor.DrawCaret(ACanvas: TCanvas);
var
  ch,delta: integer;
  el: TREElement;
  tm: TTextMetric;
  SP: TPoint;

begin
  //if (Caret.XY.Y < 0) or (Caret.XY.Y > Height) then
  //  Exit;
  if ReadOnly then
    Exit;

  SP := TopLeft;

  el := NearbyTextElement(Caret.Element, 0);

  if Assigned(el) and (el is TTextElement) then
  begin
    ACanvas.Font.Name := GetFontName((el as TTextElement).Font);
    ACanvas.Font.Size := GetFontSize((el as TTextElement).Font);
  end
  else
  begin
    if not Assigned(el) then
      el := PreviousElement(Caret.Element);

    if Assigned(el) then
    begin
      ACanvas.Font.Name := GetFontName(el.Font);
      ACanvas.Font.Size := GetFontSize(el.Font);
    end
    else
    begin
      ACanvas.Font.Name := GetFontName(Font);
      ACanvas.Font.Size := GetFontSize(Font);
    end;
  end;

  ch := ACanvas.TextHeight('gh');

  GetTextMetrics(ACanvas.Handle, TM);

  //TDebug.Write('caret descent:%d - %d %d  y:%d',[Caret.Descent, tm.tmDescent, tm.tmAscent, Caret.XY.y]);

  delta := Caret.Descent - tm.tmDescent;

  //TDebug.Write('draw caret: %d %d %d',[caret.XY.X, caret.XY.Y, Caret.LH]);

  if Caret.XY.Y + ch > 0 then
  begin
    ACanvas.Pen.Color := Caret.Color;
    ACanvas.Pen.Width := 2;
    ACanvas.Pen.Style := psSolid;
    ACanvas.MoveTo(Caret.XY.X - SP.X,Caret.XY.Y - SP.Y + Caret.LH - 1 - delta);
    ACanvas.LineTo(Caret.XY.X - SP.X,Caret.XY.Y - SP.Y + Caret.LH - ch - delta);
  end;
end;

procedure TAdvRichEditor.DrawDragCaret(ACanvas: TCanvas);
var
  sp: TPoint;
begin
  SP := TopLeft;

  if (DragCaret.XY.Y - SP.Y < 0) or (DragCaret.XY.Y + DragCaret.LH - SP.Y > Height) then
    Exit;

  ACanvas.Pen.Color := Caret.Color;
  ACanvas.Pen.Width := 2;
  ACanvas.Pen.Style := psSolid;
  ACanvas.MoveTo(DragCaret.XY.X - SP.X,DragCaret.XY.Y - SP.Y + 1);
  ACanvas.LineTo(DragCaret.XY.X - SP.X,DragCaret.XY.Y - SP.Y + DragCaret.LH - 1);
end;

function TAdvRichEditor.GetCharPos(AValue: string; CharIndex: integer): integer;
var
  nnfit,mw: integer;
  charpos: array[0..2047] of integer;
  sz: TSize;
begin
  if Assigned(Caret.Element) then
  begin
    if (Caret.Element is TTextElement) then
      Canvas.Font.Assign((Caret.Element as TTextElement).Font)
    else
      Canvas.Font.Assign(Font);

    Canvas.Font.Size := Round(Canvas.Font.Size * FZoomFactor);
  end;

  mw := GetLineWidth;
  GetTextExtentExPoint(Canvas.Handle, PChar(AValue + ' '), Min(2047,Length(AValue) + 1),mw,@nnfit,@charpos,sz);

  if (charindex > 2047)  then
    charindex := 2047;

  Result := charpos[CharIndex - 1];
end;


procedure TAdvRichEditor.DrawElement(ACanvas: TCanvas; el: TREElement; x, y, MaxLineWidth, LineHeight, LineDescent, LineBaseLine: integer; AValue: string; CharIndex: integer);
var
  r: TRect;
  tm: TTextMetric;
  pic: TGDIPPicture;
  selword: boolean;
  preValue,postValue,Value: string;
  lv,tw,pw,ph: integer;
  TEXTFLAGS: DWORD;
  iserr: boolean;
  sz: TTextSize;
  fcs: TFontCharset;
  dpi: double;
  bmp: Graphics.TBitmap;
begin
  TEXTFLAGS := DT_EDITCONTROL or DT_SINGLELINE or DT_LEFT or DT_BOTTOM or DT_EXTERNALLEADING or DT_NOPREFIX;

  {
  if (el = Selection.ToElement) then
     TDebug.Write('FROM:'+Selection.FromElement.ClassName +':' +Selection.FromElement.Text);
  if (el = Selection.ToElement) then
     TDebug.Write('TO:'+Selection.ToElement.ClassName +':' +Selection.ToElement.Text);
  }

  if (el is TBulletElement) then
  begin
    r := Rect(x,y,x + $FFFF, y + LineHeight);

    fcs := ACanvas.Font.Charset;

    ACanvas.Font.Name := 'Wingdings';
    ACanvas.Font.Charset := DEFAULT_CHARSET;

    Value := GetBulletChar((el as TBulletElement).&Type);

    case (el as TBulletElement).&Type of
    btNumber:
      begin
        Value := Format((el as TBulletElement).BulletFormat,[(el as TBulletElement).Index + 1]);
        ACanvas.Font.Name := (el as TBulletElement).BulletFont;
      end;
    btChar:
      begin
        Value := Chr(ord('a')+(el as TBulletElement).Index);
        ACanvas.Font.Name := (el as TBulletElement).BulletFont;
      end;
    btCustom:
      begin
        Value := (el as TBulletElement).Bullet;
        ACanvas.Font.Name := (el as TBulletElement).BulletFont;
      end;
    end;

    ACanvas.Brush.Style := bsClear;

    if el.Color <> clNone then
    begin
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := el.Color;
    end;

    if (el.Baseline = tbRegular) and (LineBaseLine > 0)  then // there is subscript
    begin
      sz := GetTextSize(ACanvas, el, 'gh');
      r.Bottom := r.Bottom  - round(sz.cy / 4);
    end;

    if el.Baseline = tbSuperScript then
    begin
      r.Bottom := r.Bottom  - round(el.TH / 4);
      if LineBaseLine > 0 then
        r.Bottom := r.Bottom  - round(el.TH / 4);
    end;

    ACanvas.Font.Color := el.TextColor;
    ACanvas.Font.Style := el.Font.Style;
    ACanvas.Font.Size := GetFontSize(el.Font);
    ACanvas.Font.Style := ACanvas.Font.Style - [fsUnderline, fsStrikeOut];
    DrawText(ACanvas.Handle, PChar(Value), Length(Value),r, TEXTFLAGS);

    ACanvas.Font.Charset := fcs;
  end;

  if (el is TTextElement) then
  begin
    iserr := (el as TTextElement).Error;

    ACanvas.Font.Assign((el as TTextElement).Font);
    ACanvas.Font.Color := ColorToRGB((el as TTextElement).TextColor);

    ACanvas.Font.Size := Round(ACanvas.Font.Size * FZoomFactor);

    if (el.URL <> '') then
    begin
      ACanvas.Font.Color := URLColor;
      ACanvas.Font.Style := ACanvas.Font.Style + [fsUnderline];
    end;

    GetTextMetrics(ACanvas.Handle, TM);

    y := y - (LineDescent - tm.tmDescent) + tm.tmExternalLeading;

    r := Rect(x,y,Min(x + $FFFF, MaxLineWidth), y + LineHeight);

    sz.cy := 0;

    if (el.Baseline = tbRegular) and (LineBaseLine > 0)  then // there is subscript
    begin
      r.Bottom := r.Bottom  - round(el.TH / 4);
    end;

    if el.Baseline = tbSuperScript then
    begin
      r.Bottom := r.Bottom  - round(el.TH / 4);
      if LineBaseLine > 0 then
        r.Bottom := r.Bottom  - round(el.TH / 4);
    end;

    lv := Length(AValue);

    selword := false;

    if el.Selected and not HideSelection then
    begin
      selword := true;

      if (el.SelFrom <> -1) and (CharIndex + lv <= el.SelFrom) then
        selword := false;

      if (el.SelTo <> -1) and (CharIndex >= el.SelTo) then
        selword := false;
    end;

    preValue := '';
    postValue := '';
    Value := AValue;

    if selword then
    begin
      if (CharIndex < el.SelTo) and (CharIndex + lv >= el.SelTo) then
      begin
        postValue := Copy(AValue, el.SelTo - CharIndex + 1, lv);
        Value := Copy(AValue, 1, el.SelTo - CharIndex);
      end;

      if (CharIndex < el.SelFrom) and (CharIndex + lv > el.SelFrom) then
      begin
        preValue := Copy(Value, 1, el.SelFrom - CharIndex);
        Value := Copy(Value,el.SelFrom - CharIndex + 1, lv);
      end;

      if ((el as TTextElement).Color <> clNone) then
      begin
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Brush.Color := (el as TTextElement).Color;
      end
      else
        ACanvas.Brush.Style := bsClear;

      if el.URL = '' then
        ACanvas.Font.Color := (el as TTextElement).TextColor;

      if preValue <> '' then
      begin
        if el.Highlight then
        begin
          ACanvas.Brush.Style := bsSolid;
          ACanvas.Brush.Color := clYellow;
        end;

        if el.MergeRef <> '' then
        begin
          ACanvas.Brush.Style := bsSolid;
          ACanvas.Brush.Color := clSilver;
          ACanvas.Font.Color := clBlack;
        end;

        DrawText(ACanvas.Handle, PChar(preValue), Length(preValue), r, TEXTFLAGS);
        tw := ACanvas.TextWidth(preValue);

        if iserr then
          DrawErrorLine(ACanvas, r.Left, tw, r.Bottom, clRed);

        if (el as TTextElement).Line then
        begin
          ACanvas.Pen.Color := clBlack;
          ACanvas.MoveTo(r.Left, r.Bottom);
          ACanvas.LineTo(MaxLineWidth, r.Bottom);
        end;

        r.Left := r.Left + tw;
      end;

      ACanvas.Brush.Color := SelectionColor;
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Font.Color := SelectionTextColor;

      if IsForwardSelection then
      begin
        if (el = Selection.FromElement) and (CharIndex <= Selection.FromChar) then
          Selection.FromXY := Point(r.Left, r.Top);
      end
      else
      begin
        if (el = Selection.ToElement) and (CharIndex <= Selection.ToChar) then
          Selection.FromXY := Point(r.Left, r.Top);
      end;

      if (Value <> '') then
      begin
        DrawText(ACanvas.Handle, PChar(Value), Length(Value), r, TEXTFLAGS);

        tw := ACanvas.TextWidth(Value);

        if iserr then
          DrawErrorLine(ACanvas, r.Left, tw, r.Bottom, clRed);

        if (el as TTextElement).Line then
        begin
          ACanvas.Pen.Color := clBlack;
          ACanvas.MoveTo(r.Left, r.Bottom);
          ACanvas.LineTo(MaxLineWidth, r.Bottom);
        end;

        r.Left := r.Left + tw;
      end;

      if IsForwardSelection then
      begin
        if (el = Selection.ToElement) and (CharIndex < Selection.ToChar) then
          Selection.ToXY := Point(r.Left, r.Bottom);
      end
      else
      begin
        if (el = Selection.FromElement) and (CharIndex < Selection.FromChar) then
          Selection.ToXY := Point(r.Left, r.Bottom);
      end;

      if (el as TTextElement).Color <> clNone then
      begin
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Brush.Color := (el as TTextElement).Color;
      end
      else
        ACanvas.Brush.Style := bsClear;

      if el.URL = '' then
        ACanvas.Font.Color := (el as TTextElement).TextColor
      else
        ACanvas.Font.Color := URLColor;

      if postValue <> '' then
      begin
        if el.Highlight then
        begin
          ACanvas.Brush.Style := bsSolid;
          ACanvas.Brush.Color := clYellow;
        end;

        if el.MergeRef <> '' then
        begin
          ACanvas.Brush.Style := bsSolid;
          ACanvas.Brush.Color := clSilver;
          ACanvas.Font.Color := clBlack;
        end;


        DrawText(ACanvas.Handle, PChar(postValue), Length(postValue), r, TEXTFLAGS);
        tw := ACanvas.TextWidth(postValue);

        if iserr then
          DrawErrorLine(ACanvas, r.Left, tw, r.Bottom, clRed);

        if (el as TTextElement).Line then
        begin
          ACanvas.Pen.Color := clBlack;
          ACanvas.MoveTo(r.Left, r.Bottom);
          ACanvas.LineTo(MaxLineWidth, r.Bottom);
        end;

      end;
    end
    else
    begin
      if (el as TTextElement).Color <> clNone then
      begin
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Brush.Color := (el as TTextElement).Color;
      end
      else
        ACanvas.Brush.Style := bsClear;

      if el.Highlight then
      begin
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Brush.Color := clYellow;
      end;

      if el.MergeRef <> '' then
      begin
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Brush.Color := clSilver;
        ACanvas.Font.Color := clBlack;
      end;

      DrawText(ACanvas.Handle, PChar(AValue), Length(AValue), r, TEXTFLAGS);

      if iserr then
      begin
        tw := ACanvas.TextWidth(AValue);
        DrawErrorLine(ACanvas, r.Left, tw, r.Bottom, clRed);
      end;

      if (el as TTextElement).Line then
      begin
        ACanvas.Pen.Color := clBlack;
        ACanvas.MoveTo(r.Left, r.Bottom);
        ACanvas.LineTo(MaxLineWidth, r.Bottom);
      end;

    end;
  end
  else
  begin
    if el = Selection.FromElement then
    begin
      Selection.FromXY := Point(x, y);
    end;

    if el = Selection.ToElement then
    begin
      Selection.ToXY := Point(x,y + LineHeight);
    end;
  end;

  if (el is TLineElement) then
  begin
    ACanvas.Pen.Color := (el as TLineElement).Color;
    ACanvas.Pen.Width := (el as TLineElement).Width;
    ACanvas.Pen.Style := (el as TLineElement).Style;
    ACanvas.MoveTo(0, y + LineHeight div 2);
    ACanvas.LineTo(MaxLineWidth, y + LineHeight div 2);
  end;

  if (el is TCustomGraphicElement) then
  begin
    dpi := (el as TCustomGraphicElement).DPIratio;

    r := Rect(x,y,x + Round((el as TCustomGraphicElement).Size.cx * dpi), y + Round((el as TCustomGraphicElement).Size.cy * dpi));

    InflateRect(r, -Round(PICTURE_MARGIN * dpi) div 2, -Round(PICTURE_MARGIN * dpi) div 2);

    if el.Selected and not HideSelection and HasSelection then
    begin
      ACanvas.Brush.Color := SelectionColor;
      ACanvas.Pen.Color := SelectionColor;
      ACanvas.Rectangle(r);
    end;

    if dpi <> 1.0 then
    begin
      ACanvas.Brush.Color := clWhite;
      ACanvas.Pen.Color := clWhite;
      ACanvas.Rectangle(r);
    end;

    pic := nil;
    pw := 0;
    ph := 0;

    {$IFDEF TMSPACK}
    if (el is TNamedPictureElement) then
    begin
      if (el is TEmoticonPictureElement) then
        pic := GetPictureByName(Emoticons, (el as TEmoticonPictureElement).Name)
      else
        pic := GetPictureByName(PictureContainer, (el as TNamedPictureElement).Name);

      if not Assigned(pic) then
      begin
        // No handler implemented for custom graphic
        ACanvas.Pen.Color := clRed;
        ACanvas.Pen.Width := 1;
        ACanvas.Pen.Style := psSolid;

        ACanvas.MoveTo(r.Left,r.Top);
        ACanvas.LineTo(r.Right,r.Bottom);

        ACanvas.MoveTo(r.Right, r.Top);
        ACanvas.LineTo(r.Left,r.Bottom);
      end
      else
      begin
        (el as TNamedPictureElement).Width := pic.Width;
        (el as TNamedPictureElement).Height := pic.Height;
      end;
    end;
    {$ENDIF}

    if (el is TPictureElement) then
    begin
      pic := (el as TPictureElement).Picture;
      ph := (el as TPictureElement).PictureHeight;
      pw := (el as TPictureElement).PictureWidth;
    end;

    {$IFNDEF LCLLIB}
    if (el is TPictureElement) or (el is TNamedPictureElement) then
    begin
      if Assigned(pic) and not pic.Empty then
      begin
        if (dpi <> 1.0) then
        begin
          bmp := Graphics.TBitmap.Create;
          bmp.Transparent := false;
          bmp.Width := r.Right - r.Left;
          bmp.Height := r.Bottom - r.Top;
          bmp.Canvas.Brush.Color := clWhite;
          bmp.Canvas.Rectangle(r);
          bmp.Canvas.StretchDraw(Rect(0,0,bmp.Width,bmp.Height),pic);
          ACanvas.Draw(r.Left,r.Top,bmp);
          bmp.Free;
        end
        else
        begin
          if (pw > 0) and (ph > 0) then
          begin
            ACanvas.StretchDraw(r, pic);
          end
          else
          begin
            if dpi <> 1.0 then
              ACanvas.StretchDraw(r, pic)
            else
              ACanvas.Draw(x + PICTURE_MARGIN div 2,y, pic);
          end;
        end;
      end;
    end;
    {$ENDIF}

    if (el is TGraphicElement) then
    begin
      if not DrawGraphic(ACanvas, r, (el as TGraphicElement).ID) then
      begin
        // No handler implemented for custom graphic
        ACanvas.Pen.Color := clRed;
        ACanvas.Pen.Width := 1;
        ACanvas.Pen.Style := psSolid;

        ACanvas.MoveTo(r.Left,r.Top);
        ACanvas.LineTo(r.Right,r.Bottom);

        ACanvas.MoveTo(r.Right, r.Top);
        ACanvas.LineTo(r.Left,r.Bottom);
      end;
    end;

    if (el = Selected) then
    begin
      DrawSelection(ACanvas, r);
    end;

  end;
end;

procedure TAdvRichEditor.DrawErrorLine(ACanvas: TCanvas; x, w, y: integer; AColor: TColor);
var
  l,o: integer;
begin
  ACanvas.Pen.Color := AColor;
  ACanvas.Pen.Width := 1;

  l := (x div 2) * 2;
  if (l mod 4)=0 then o := 2 else o := 0;

  ACanvas.MoveTo(l,y + o - 1);

  while l < x + w do
  begin
    if o = 2 then o := 0 else o := 2;
    ACanvas.LineTo(l + 2,y + o - 1);
    Inc(l,2);
  end;
end;

procedure TAdvRichEditor.DrawLineBreak(ACanvas: TCanvas; x,y:integer; el: TREElement);
begin
  ACanvas.Brush.Color := SelectionColor;
  ACanvas.Pen.Color := ACanvas.Brush.Color;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Pen.Style := psSolid;

  ACanvas.Rectangle(x - 1, y, x + 1, y + el.LH);

  if IsForwardSelection then
  begin
    if Selection.FromElement = el then
      Selection.FromXY := el.XY;

    if Selection.ToElement = el then
      Selection.ToXY := Point(el.XY.X ,el.XY.Y + el.LH);
  end
  else
  begin
    if Selection.ToElement = el then
      Selection.FromXY := el.XY;

    if Selection.FromElement = el then
      Selection.ToXY := Point(el.XY.X ,el.XY.Y + el.LH);
  end;
end;

function TAdvRichEditor.DrawGraphic(ACanvas: TCanvas; ARect: TRect; ID: string): boolean;
begin
  Result := Assigned(OnDrawGraphic);

  if Assigned(OnDrawGraphic) then
    OnDrawGraphic(Self, ACanvas, ARect, ID)
end;

procedure TAdvRichEditor.DrawMargin(ACanvas: TCanvas);
var
  R: TRect;
begin
  R := ClientRect;
  R.Bottom := PageMargin.Vertical;

  ACanvas.Brush.Color := PageMargin.Color;
  ACanvas.Pen.Color := PageMargin.Color;

  ACanvas.FillRect(r);

  R := ClientRect;
  R.Right := PageMargin.Horizontal;
  ACanvas.FillRect(r);

  R := ClientRect;
  R.Left := R.Right - PageMargin.Horizontal;
  ACanvas.FillRect(r);
end;

procedure TAdvRichEditor.DrawSelection(ACanvas: TCanvas; r: TRect);
begin
  ACanvas.Brush.Color := GraphicSelection.Color;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Pen.Color := GraphicSelection.BorderColor;
  ACanvas.Pen.Style := psSolid;

  if GraphicSelection.Style = gsCircle then
  begin
    ACanvas.Ellipse(r.Left - 3,r.Top - 3, r.Left + 3, r.Top + 3);
    ACanvas.Ellipse(r.Left - 3,r.Bottom - 3, r.Left + 3, r.Bottom + 3);
    ACanvas.Ellipse(r.Right - 3,r.Top - 3, r.Right + 3, r.Top + 3);
    ACanvas.Ellipse(r.Right - 3,r.Bottom - 3, r.Right + 3, r.Bottom + 3);
  end
  else
  begin
    ACanvas.Rectangle(r.Left - 3,r.Top - 3, r.Left + 3, r.Top + 3);
    ACanvas.Rectangle(r.Left - 3,r.Bottom - 3, r.Left + 3, r.Bottom + 3);
    ACanvas.Rectangle(r.Right - 3,r.Top - 3, r.Right + 3, r.Top + 3);
    ACanvas.Rectangle(r.Right - 3,r.Bottom - 3, r.Right + 3, r.Bottom + 3);
  end;
end;

procedure TAdvRichEditor.EndPaintBuffer;
begin
  Canvas.Draw(0, 0, Buffer);
end;

function TAdvRichEditor.GetCharInWord(el: TREElement; s: string; X: integer): integer;
var
  mw: integer;
  nnfit: integer;
  sz: TSize;

begin
  if X < 2 then
    Result := 0
  else
  begin
    mw := X + Canvas.TextWidth('n') div 2;
    Canvas.Font.Assign(el.Font);
    Canvas.Font.Size := Round(Canvas.Font.Size * FZoomFactor);

    GetTextExtentExPoint(Canvas.Handle, PChar(s), Length(s), mw, @nnfit, nil, sz);
    Result := Max(0, nnfit);
  end;
end;

function TAdvRichEditor.GetDefaultFont: TFont;
begin
  Result := Font;
end;

function TAdvRichEditor.GetDefaultFontColor: TColor;
begin
  Result := Font.Color;
end;

function TAdvRichEditor.GetLineHeight(AElement: TREElement): integer;
var
  R: TRect;
begin
  if not Assigned(AElement) then
  begin
    Canvas.Font.Assign(GetDefaultFont);
  end
  else
    Canvas.Font.Assign(AElement.Font);

  R := Rect(0,0,1000,1000);
  Result := DrawText(Canvas.Handle, 'gh',2,R,DT_LEFT or DT_CALCRECT);
end;

function TAdvRichEditor.SetRange(ARange: TSize): boolean;
var
  sz: TSize;
begin
  sz := ARange;
  if SingleLine then
    sz.cy := Height - 4;

  Result := inherited SetRange(sz);
end;

procedure TAdvRichEditor.SetRichEditorPopup(AValue: TAdvRichEditorPopup);
begin
  FRichEditorPopup := AValue;
  if Assigned(FRichEditorPopup) then
    FRichEditorPopup.RichEditor := Self;
end;

function TAdvRichEditor.GetClientHeight: integer;
begin
  if HandleAllocated then
    Result := ClientHeight
  else
    Result := Height;
end;

function TAdvRichEditor.GetClientWidth: integer;
begin
  if HandleAllocated then
    Result := ClientWidth
  else
    Result := Width;
end;

{$IFDEF TMSPACK}
function TAdvRichEditor.GetPictureByName(Container: TGDIPPictureContainer; const AName: string): TGDIPPicture;
var
  st: TMemoryStream;
  ap: TAdvGDIPPicture;
begin
  Result := nil;

  if Assigned(Container) then
  begin
    ap := Container.FindPicture(AName);

    if Assigned(ap) then
    begin
      Result := FGDIPPicture;
      st := TMemoryStream.Create;
      try
        ap.SaveToStream(st);
        st.Position := 0;
        Result.LoadFromStream(st);
      finally
        st.Free;
      end;
    end;
  end;
end;
{$ENDIF}

function TAdvRichEditor.GetPictureSize(el: TREElement): TSize;
var
  pic: TGDIPPicture;
  sz: TSize;

begin
  sz.cx := 0;
  sz.cy := 0;

  if (el is TPictureElement) then
  begin
    if ((el as TPictureElement).PictureWidth > 0) and ((el as TPictureElement).PictureHeight > 0) then
    begin
      sz.cx := (el as TPictureElement).PictureWidth;
      sz.cy := (el as TPictureElement).PictureHeight;
    end
    else
    begin
      pic := (el as TPictureElement).Picture;

      if Assigned(pic) and not pic.Empty then
      begin
       sz.cx := pic.Width;
       sz.cy := pic.Height;
      end;
    end;
  end;

  sz.cx := Round(sz.cx * DPIratio);
  sz.cy := Round(sz.cy * DPIratio);

  Result := sz;
end;

function TAdvRichEditor.GetTextDescent(ACanvas: TCanvas; el: TREElement): integer;
var
  tm: TTextMetric;
begin
  GetTextMetrics(ACanvas.Handle, TM);
  Result := tm.tmDescent;
end;

function TAdvRichEditor.GetTextSize(ACanvas: TCanvas; el: TREElement; AValue: string): TTextSize;
begin
  if Assigned(el) then
    ACanvas.Font.Assign(el.Font)
  else
    ACanvas.Font.Assign(GetDefaultFont);

  Result.cx := Round(ACanvas.TextWidth(AValue) * FZoomFactor);
  Result.cy := Round(ACanvas.TextHeight('gh') * FZoomFactor);

  Result.sup := 0;
  Result.sub := 0;

  if Assigned(el) then
  begin
    if (el.Baseline = tbSubscript) then
      Result.sub := Round(Result.cy/4);

    if (el.Baseline = tbSuperScript) then
      Result.sup := Round(Result.cy/4);
  end;

end;

function TAdvRichEditor.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' +
    IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvRichEditor.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

function TAdvRichEditor.GetVersionString: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)))+' '+DATE_VER;
end;

function TAdvRichEditor.GetBulletChar(const AType: TBulletType): char;
begin
  Result := #0;

  case AType of
  btCircle: Result := #$9F;
  btSquare: Result := #$A7;
  btStar: Result := #$AB;
  btArrow: Result := #$E0;
  btTick: Result := #$FC;
  end;
end;

function TAdvRichEditor.GetBulletSize(el: TREElement; AValue: string): TSize;
begin
  if Assigned(el) then
    Canvas.Font.Assign(el.Font)
  else
    Canvas.Font.Assign(GetDefaultFont);

  Canvas.Font.Name := 'Wingdings';

  Result.cx := Canvas.TextWidth(AValue);

  if AValue = '' then
    AValue := 'gh';

  Result.cy := Canvas.TextHeight(AValue);


  if (el is TBulletElement) then
    Result.cx := Result.cx + (el as TBulletElement).Spacing;
end;

procedure TAdvRichEditor.InitVCLStyle(init: boolean);
{$IFDEF DELPHIXE2_LVL}
var
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;
  clr: TColor;
{$ENDIF}
begin
  UseVCLStyles := False;

{$IFDEF DELPHIXE2_LVL}
  LStyle := StyleServices;

  if LStyle.Enabled and (LStyle.Name <> 'Windows') then
  begin
    UseVCLStyles := True;

    LDetails := LStyle.GetElementDetails(tgCellNormal);
    LStyle.GetElementColor(LDetails, ecFillColor, clr);
    Color := clr;

    LStyle.GetElementColor(LDetails, ecBorderColor, clr);
    BorderColor := clr;

    LStyle.GetElementColor(LStyle.GetElementDetails(tgFixedCellNormal), ecBorderColor, clr);

    if LStyle.GetElementColor(LStyle.GetElementDetails(teEditTextNormal), ecTextColor, clr) and (clr <> clNone) then
      Font.Color := clr;

    if LStyle.GetElementColor(LStyle.GetElementDetails(teEditTextSelected), ecFilLColor, clr) and (clr <> clNone) then
    begin
      SelectionColor := clr;
    end
    else
      SelectionColor := clHighlight;

    if LStyle.GetElementColor(LStyle.GetElementDetails(teEditTextSelected), ecTextColor, clr) and (clr <> clNone) then
    begin
      SelectionTextColor := clr;
    end
    else
      SelectionTextColor := clHighlightText;
  end
  else
  begin
    if init then
    begin
      Color := clWindow;
      BorderColor := clGray;
      Font.Color := clBlack;
      SelectionColor := clHighlight;
      SelectionTextColor := clHighlightText;
    end;
  end;
{$ENDIF}
end;

procedure TAdvRichEditor.InitContextMenu;
var
  mnu: TMenuItem;
  alignmnu: TMenuItem;

  {$IFNDEF LCLLIB}
  procedure AddImageListImage(AName: string);
  var
    pngbmp: TPngImage;
    bmp: Graphics.TBitmap;
  begin
    pngbmp := TPNGImage.Create;
    bmp := Graphics.TBitmap.Create;
    try
      pngbmp.LoadFromResourceName(HInstance,AName);
      pngbmp.AssignTo(bmp);
      bmp.AlphaFormat := afIgnored;
      ImageList_Add(FImageList.Handle, bmp.Handle, 0);
    finally
      pngbmp.Free;
      bmp.Free;
    end;
  end;
  {$ENDIF}

begin
  FImageList.Masked := false;

  {$IFNDEF LCLLIB}
  FImageList.ColorDepth := cd32bit;

  AddImageListImage('TMSRETBCUT');
  AddImageListImage('TMSRETBCOPY');
  AddImageListImage('TMSRETBPASTE');
  {$ENDIF}

  FPopupMenu.Images := FImageList;

  mnu := TMenuItem.Create(FPopupMenu);
  mnu.Caption := 'Clear';
  mnu.ImageIndex := -1;
  mnu.Action := FClearAction;
  FPopupMenu.Items.Add(mnu);

  mnu := TMenuItem.Create(FPopupMenu);
  mnu.Caption := '-';
  mnu.ImageIndex := -1;
  FPopupMenu.Items.Add(mnu);

  mnu := TMenuItem.Create(FPopupMenu);
  mnu.ImageIndex := 0;
  mnu.Action := FCutAction;
  FPopupMenu.Items.Add(mnu);

  mnu := TMenuItem.Create(FPopupMenu);
  mnu.ImageIndex := 1;
  mnu.Action := FCopyAction;
  FPopupMenu.Items.Add(mnu);

  mnu := TMenuItem.Create(FPopupMenu);
  mnu.ImageIndex := 2;
  mnu.Action := FPasteAction;
  FPopupMenu.Items.Add(mnu);

  mnu := TMenuItem.Create(FPopupMenu);
  mnu.Caption := '-';
  mnu.ImageIndex := -1;
  FPopupMenu.Items.Add(mnu);

  alignmnu := TMenuItem.Create(FPopupMenu);
  alignmnu.Caption := 'Align';
  alignmnu.ImageIndex := -1;
  FPopupMenu.Items.Add(alignmnu);

  mnu := TMenuItem.Create(FPopupMenu);
  mnu.ImageIndex := -1;
  mnu.Action := FLeftAction;
  alignmnu.Add(mnu);

  mnu := TMenuItem.Create(FPopupMenu);
  mnu.ImageIndex := -1;
  mnu.Action := FCenterAction;
  alignmnu.Add(mnu);

  mnu := TMenuItem.Create(FPopupMenu);
  mnu.ImageIndex := -1;
  mnu.Action := FRightAction;
  alignmnu.Add(mnu);
end;

procedure TAdvRichEditor.InitializePictureSize(
  NamedPictureElement: TNamedPictureElement);
{$IFDEF TMSPACK}
var
  pic: TGDIPPicture;
{$ENDIF}
begin
  {$IFDEF TMSPACK}
  pic := GetPictureByName(Emoticons, NamedPictureElement.Name);

  if Assigned(pic) then
  begin
    NamedPictureElement.Width := pic.Width;
    NamedPictureElement.Height := pic.Height;
  end
  else
  begin
    NamedPictureElement.Width := 0;
    NamedPictureElement.Height := 0;
  end;
  {$ENDIF}
end;

procedure TAdvRichEditor.KeyDown(var Key: Word; Shift: TShiftState);
var
  SP: TPoint;
  AState: TCharCommandState;
begin
  if ReadOnly then
  begin
    inherited;
    Exit;
  end;

  SP := TopLeft;
  EnsureCaret;

  FDoCaret := true;
  FCaretTimer.Enabled := false;

  AState := ccNormal;

  if (ssShift in Shift) and not (ssCtrl in Shift) then
    AState := ccShift;

  if (ssShift in Shift) and (ssCtrl in Shift) then
    AState := ccCtrlShift;

  if not (ssShift in Shift) and (ssCtrl in Shift) and not (ssAlt in Shift) then
    AState := ccCtrl;

  if (Key in [VK_DELETE,VK_LEFT,VK_RIGHT,VK_PRIOR,VK_NEXT]) then
  begin
    CharCommand(Key, AState);
  end;

  if (Key in [VK_A,VK_C,VK_V,VK_X,VK_Z,VK_Y,VK_B,VK_I,VK_U,VK_E,VK_R,VK_L]) and (AState = ccCtrl) then
  begin
    CharCommand(Key, AState);
  end;

  if (Key in [VK_UP,VK_DOWN,VK_HOME,VK_END,VK_INSERT]) then
  begin
    CharCommand(Key, AState);
  end;

  inherited;
end;

procedure TAdvRichEditor.KeyPress(var Key: Char);
var
  AType: TBulletType;
  AIndex, AIndent: integer;
  IsShift: boolean;
begin
  inherited;

  EnsureCaret;

  if ReadOnly then
    Exit;

  if (Key = #13) and not SingleLine then
  begin
    IsShift := GetKeyState(VK_SHIFT) and $8000 = $8000;
    if not IsShift and IsCaretInBulletList(AType, AIndex, AIndent) then
      InsertLineBreakAndBullet(AType, AIndent)
    else
      InsertLineBreak(IsShift);
  end
  else
  if Key = #32 then
  begin
    InsertChar(Key);
  end
  else
  if Key = #8 then
  begin
    Backspace;
  end
  else
  if Key = #9 then
  begin
    InsertTab;
  end
  else
  if Key = #$16 then
  begin
    // Ctrl-V
  end
  else
  if Key = #$18 then
  begin
    // Ctrl-X
  end
  else
  if Key = #$1A then
  begin
    // Ctrl-X
  end
  else
  if (Ord(Key) < $20) then
  begin
    // Ctrl-A, Ctrl-B, Ctrl-C, Ctrl-I, Ctrl-U
  end
  else
    InsertChar(Key);
end;

procedure TAdvRichEditor.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  FCaretTimer.Enabled := true;
end;

procedure TAdvRichEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  CurCaretEl,el: TREElement;
  CurCaretChar: integer;
  SP: TPoint;
  hasElem: boolean;
  IsSel,CanSelect: boolean;
begin
  inherited;

  if not Enabled then
    Exit;

  if (Button <> mbLeft) then
    Exit;

  FClickOnSel := false;
  CurCaretEl := Caret.Element;
  CurCaretChar := Caret.CharIndex;

  if not FDoImageResize then
  begin
    FDoImageResize := (Cursor = crSizeNESW) or (Cursor = crSizeNWSE);
    if FDoImageResize then
      PushContext;
  end;

  if Assigned(PopupToolBar) and PopupToolBar.Visible then
    PopupToolBar.Hide;

  FMouseDown := true;

  SetFocus;

  FDownPos := Point(X,Y);
  FDownXY := Point(X,Y);

  if not (ssAlt in Shift) then
  begin
    XYToCaret(X,Y, True, IsSel);

    if IsSel and FAllowSelect then
    begin
      FClickSelXY := Point(X,Y);
      FClickOnSel := true;

      if (Caret.Element is TCustomGraphicElement) then
      begin
        CanSelect := true;
        DoCanSelectGraphic(Caret.Element, CanSelect);

        if CanSelect then
        begin
          Selected := Caret.Element;
          Selection.FromElement := Caret.Element;
          Selection.ToElement := Caret.Element;
          Refresh;
          DoSelectionChanged;
        end;

        DoClickGraphic(Caret.Element);
      end;

      Exit;
    end;
  end;

  XYToCaret(X,Y,False,IsSel);

  if ((ssCtrl in Shift) or not FAllowSelect) and Assigned(Caret.Element) and (Caret.Element.URL <> '') then
  begin
    DoClickHyperlink(Caret.Element.URL, Caret.Element.Text);
  end;

  if not FAllowSelect then
  begin
    FMouseDown := false;
    Exit;
  end;

  SetCapture(Handle);

  FDoCaret := true;
  FCaretTimer.Enabled := false;

  SP := TopLeft;

  hasElem := XYToElement(X + sp.X,Y + sp.Y,el);

  Selected := nil;

  if hasElem and (el is TTabElement) then
  begin
    CanSelect := true;
    if CanSelect then
    begin
      Selected := el;
      Selection.FromElement := el;
      Selection.ToElement := el;
      Refresh;
      DoSelectionChanged;
    end;
  end;

  if hasElem and (el is TCustomGraphicElement) then
  begin
    CanSelect := true;
    DoCanSelectGraphic(el, CanSelect);

    FDownSize := (el as TCustomGraphicElement).Size;

    if CanSelect then
    begin
      Selected := el;
      Selection.FromElement := el;
      Selection.ToElement := el;
      Refresh;
      DoSelectionChanged;
    end;

    DoClickGraphic(el);
  end
  else
  if (ssShift in Shift) then
  begin
    Selection.FromElement := CurCaretEl;
    Selection.FromChar := CurCaretChar;
    Selection.ToElement := Caret.Element;
    Selection.ToChar := Caret.CharIndex;

    DoSelectionChanged;
  end;
end;

procedure TAdvRichEditor.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  el,nel: TREElement;
  idx: integer;
  dx,dy: integer;
  s: string;
  SP,PT: TPoint;
  hasElem, CanSize: boolean;
  r: TRect;
  inarea: boolean;
  CX, CY: integer;
  ar: double;
  rtf,txt: string;
  ms: TMemoryStream;
  dwEffects: Integer;
  graphicresize: boolean;
  mouseinsel: boolean;

begin
  inherited;

  SP := TopLeft;

  inarea := (x >= PageMargin.Horizontal) and (y >= PageMargin.Vertical) and (x <= Width - PageMargin.Horizontal);

  if x < PageMargin.Horizontal then
    x := PageMargin.Horizontal + 3;
  if y < PageMargin.Vertical then
    y := PageMargin.Vertical + 3;
  if x > Width - PageMargin.Horizontal then
    x := Width - PageMargin.Horizontal;
  if y > Height then
    y := Height;

  hasElem := XYToElement(X + sp.X,Y + sp.Y,el) or (Assigned(Selected));

  if hasElem and ShowHint then
  begin
    if (FLastHint <> el.Hint) and (el.Hint <> '') then
    begin
      FLastHint := el.Hint;
      Application.CancelHint;
    end;
  end;

  if HasSelection and not FMouseDown and
     Assigned(Selection.FromElement) and Assigned(Selection.ToElement) and
     Assigned(PopupToolBar) then
  begin
    mouseinsel := false;

    if (y >= Selection.FromXY.Y) and (y <= Selection.ToXY.Y) then
    begin
      mouseinsel := (x >= Selection.FromXY.X) and (x <= Selection.ToXY.X);
    end;

    //outputdebugstring(pchar(inttostr(selection.FromXY.X)+':'+inttostr(Selection.FromXY.Y)+' - '+inttostr(Selection.ToXY.X)+':'+inttostr(selection.ToXY.Y)));

    if not PopupToolBar.Visible and mouseinsel then
    begin
      PT := ClientToScreen(Point(Selection.FromXY.X, Selection.FromXY.Y - 44));
      PopupToolBar.RichEditor := Self;
      PopupToolBar.Show(PT);
    end;

    if PopupToolBar.Visible and not mouseinsel then
    begin
      if not PopupToolBar.MouseInPopup(ClientToScreen(Point(X,Y))) then
      PopupToolBar.Hide;
    end;
  end;

  graphicresize := Assigned(Selected) and (Selected is TCustomGraphicElement) and FDoImageResize;

  if FMouseDown and FClickOnSel and not graphicresize then
  begin
    if ( (Abs(X - FClickSelXY.X) > 4) or (Abs(Y - FClickSelXY.Y) > 4)) then
    begin
      {$IFNDEF LCLLIB}
      FDropSource := TAdvRichEditorDropSource.Create(Self);

      rtf := GetContentAsRTF(true);
      txt := SelectedText;
      if (txt = '') and ElementsSelected then // bitmap handling when no text is involved causes problem
        txt := '*';

      ms := TMemoryStream.Create;

      try
        SaveSelectionToStream(ms);
        ms.Position := 0;
        FInternalDD := true;

        if not (StartTextDoDragDrop(FDropSource,txt,rtf,ms,DROPEFFECT_COPY or DROPEFFECT_MOVE,dwEffects) = DRAGDROP_S_CANCEL) then
        begin
          DragCaret.Element := nil;
        end;

      finally
        FInternalDD := false;
        ms.Free;
        FClickOnSel := false;
        FMouseDown := false;
      end;
      {$ENDIF}
    end;

    Exit;
  end;

  (*
  if (ssCtrl in Shift) then                                                                                    *
  begin
    XYToChar(X + SP.X,Y + sp.Y,el, CX, CY);
    DragCaret.XY := Point(CX - sp.X, CY- sp.Y);
    DragCaret.LH := 16;
    //TDebug.Write('drag caret: %d %d',[DragCaret.XY.X, DragCaret.XY.Y]);
    Repaint;
    Exit;
  end;
  *)

  if FMouseDown then
  begin
    if hasElem then
    begin
      if not FDoImageResize then
      begin
        if (el is TTextElement) then
        begin
          s := (el as TTextElement).DisplayText;

          idx := XYToChar(X + SP.X,Y + sp.Y,el, CX, CY);

          if idx > -1 then
          begin
            Selection.ToElement := el;
            Selection.ToChar := idx;
            Caret.Element := el;
            Caret.CharIndex := idx;
            Update;
          end;
        end;

        if (el is TCustomGraphicElement) or (el is TBulletStart) then
        begin
          Selection.ToElement := el;
          Selection.ToChar := 0;
          Update;
        end;

        if (el is TTabElement) then
        begin
          Selection.FromElement := el;
          Selection.FromChar := Caret.CharIndex;
          Selection.ToElement := el;
          Selection.ToChar := Caret.CharIndex;
          Update;
        end;

        if (el is TLineBreakElement) then
        begin
          if (Y + SP.Y > el.XY.Y + el.LH) then
          begin
            nel := NextElement(el);

            while Assigned(nel) and ((nel is TBulletStart) or (nel is TBulletElement)) do
              nel := NextElement(nel);

            if Assigned(nel) and not (nel is TLineBreakElement) then
            begin
              Selection.ToElement := nel;
              Selection.ToChar := 0;
            end
            else
              Caret.CharIndex := 1;
          end
          else
          begin
            Selection.ToElement := el;
            Selection.ToChar := 0;
          end;
          Update;
        end;
      end;

      if (Selected is TCustomGraphicElement) then
      begin
        if FDoImageResize then
        begin
          dx := X - FDownXY.X;
          dy := Y - FDownXY.Y;

          case FImageResizeCorner of
          irTopLeft:
            begin
              dx := -dx;
              dy := -dy;
            end;
          irTopRight:
            begin
              dy := -dy;
            end;
          irBottomLeft:
            begin
              dx := -dx;
            end;
          end;

          if (dx <> 0) or (dy <> 0) then
          begin

            if (Selected is TPictureElement) then
            begin
              // sizing with aspect ratio
              if (ssCtrl in Shift) then
              begin
                ar := FDownSize.cx / FDownSize.cy;

                if dx > dy then
                begin
                  dy := Round(dx * ar);
                end
                else
                begin
                  dx := Round(dy * ar);
                end;
              end;

              if (FDownSize.cx  + dx > 0) then
                (Selected as TPictureElement).PictureWidth := FDownSize.cx  + dx;
              if (FDownSize.cy  + dy > 0) then
                (Selected as TPictureElement).PictureHeight := FDownSize.cy  + dy;

              Update;
            end;

            if (Selected is TGraphicElement) then
            begin
              if ((Selected as TGraphicElement).Width  + dx > 0) then
                (Selected as TGraphicElement).Width := (Selected as TGraphicElement).Width  + dx;
              if ((Selected as TGraphicElement).Height  + dy > 0) then
                (Selected as TGraphicElement).Height := (Selected as TGraphicElement).Height  + dy;

            end;

            FDownPos := Point(X,Y);
            Refresh;
          end;
        end
        else
        begin
          Selection.ToElement := el;

          if X > el.XY.X + el.Size.cx div 2 then
            Selection.ToChar := 1
          else
            Selection.ToChar := 0;

          Update;
        end;
      end;
    end;

    Caret.Element := Selection.ToElement;
    Caret.CharIndex := Selection.ToChar;
    Refresh;

    if (y < 20) and not FSingleLine then
      ScrollUp(20);

    if (Y > Height - 20) and not FSingleLine then
      ScrollDown(20);
  end
  else
  begin
    if hasElem then
    begin
      if (el is TCustomGraphicElement) then
      begin
        if (el.URL <> '') and (x < el.XYE.X) and (y < el.XYE.Y) then
          Cursor := crHandPoint
        else
        begin
          Cursor := crDefault;

          CanSize := true;
          DoCanSizeGraphic(el, CanSize);

          if CanSize then
          begin
            r := Rect(el.XY.X - 3 - SP.X, el.XY.Y - 3 , el.XY.X + 3 - SP.X, el.XY.Y + 3 );
            if PtInRect(r, Point(x + SP.X,y + SP.Y)) then
            begin
              Cursor := crSizeNWSE;
              FImageResizeCorner := irTopLeft;
            end;

            r := Rect(el.XY.X + el.Size.cx - 3 - SP.X, el.XY.Y - 3 , el.XY.X + el.Size.cx + 3 - SP.X, el.XY.Y + 3 );
            if PtInRect(r, Point(x + SP.X,y + SP.Y)) then
            begin
              Cursor := crSizeNESW;
              FImageResizeCorner := irTopRight;
            end;

            r := Rect(el.XY.X + el.Size.cx  + - 3 - SP.X, el.XY.Y + el.Size.cy - 3 , el.XY.X + el.Size.cx + 3 - SP.X, el.XY.Y + el.Size.cy + 3 );
            if PtInRect(r, Point(x + SP.X,y + SP.Y)) then
            begin
              Cursor := crSizeNWSE;
              FImageResizeCorner := irBottomRight;
            end;

            r := Rect(el.XY.X - 3 - SP.X, el.XY.Y + el.Size.cy - 3, el.XY.X + 3 - SP.X, el.XY.Y + el.Size.cy + 3 );
            if PtInRect(r, Point(x + SP.X,y + SP.Y)) then
            begin
              Cursor := crSizeNESW;
              FImageResizeCorner := irBottomLeft;
            end;
          end;
        end;
      end
      else
        if (el.URL <> '') and (x < el.XYE.X) and (y < el.XYE.Y) then
          Cursor := crHandPoint
        else
          Cursor := crIBeam;
    end
    else
      Cursor := crIBeam;
  end;

  if not inarea then
    Cursor := crDefault;

  if not FAllowSelect and (Cursor = crIBeam) then
    Cursor := crDefault;
end;

procedure TAdvRichEditor.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  IsSel: boolean;
begin
  inherited;

  if FClickOnSel then
  begin
    XYToCaret(X,Y,False,IsSel);
  end;

  FDoImageResize := false;
  FMouseDown := false;
  FCaretTimer.Enabled := true;
  ReleaseCapture;
end;

procedure TAdvRichEditor.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
{$IFDEF TMSPACK}
  if (AComponent = FGDIPPictureContainer) and (AOperation = opRemove) then
    FGDIPPictureContainer := nil;
  if (AComponent = FEmoticons) and (AOperation = opRemove) then
    FEmoticons := nil;
{$ENDIF}

  if (AComponent = FRichEditorPopup) and (AOperation = opRemove) then
    FRichEditorPopup := nil;
end;

procedure TAdvRichEditor.Paint;
{$IFDEF FREEWARE}
var
  fw: string;
{$ENDIF}
begin
  inherited;

  if FDoCaret and Focused then
    DrawCaret(Canvas);

  if Assigned(DragCaret.Element) then
    DrawDragCaret(Canvas);

  {$IFDEF FREEWARE}
  fw := ClassName  + trialversion + GetVersionString;
  Canvas.Font.Color := clRed;
  Canvas.Brush.Style := bsClear;
  Canvas.TextOut(4,Height - 20,fw);
  {$ENDIF}
end;

function TAdvRichEditor.PasteFromClipboard: string;
var
  pic: TPictureElement;
  hDrop, hClip: THandle;
  count,c,ln,i,lp: integer;
  AFilename,AExt,su: string;
  bmp: Graphics.TBitmap;
  bufptr: pointer;
  mstream: TStringStream;
  htmlvalue,rtfvalue: string;
  didHTML, didRTF, Allow: boolean;
  sl: TStringList;
begin
  didHTML := false;
  didRTF := false;

  if Clipboard.HasFormat(ClipboardRTEFMT) and (cfRTE in ClipboardFormats) then
  begin
    PasteFormattedSelectionFromClipboard;
    Exit;
  end;

  if Clipboard.HasFormat(ClipboardRTFFMT) and (cfRTF in ClipboardFormats) then
  begin
    Clipboard.Open;

    {$IFNDEF LCLLIB}
    HClip := Clipboard.GetAsHandle(ClipboardRTFFMT);
    bufptr := GlobalLock(HClip);
    if bufptr <> nil then
    begin
      try
        mstream := TStringStream.Create('');
        try
          mstream.WriteBuffer(bufptr^, GlobalSize(HClip));

          sl := TStringList.Create;
          sl.Text := UTF8ToString(RawByteString(mstream.DataString));

          mstream.Clear;
          try
            for i := 0 to sl.Count - 1 do
            begin
              rtfvalue := sl.Strings[i];
              if (Length(rtfvalue) > 0) and (CharInStr(rtfvalue,1) <> '\') then
                mstream.WriteString(#13);
              mstream.WriteString(rtfvalue);

              lp := Length(rtfvalue);
              su := '';
              if lp >= 2 then
                su := Copy(rtfvalue, lp - 1, 2);

              if su = ' \' then
                mstream.WriteString('line');
            end;
          finally
            sl.Free;
          end;

          if HasSelection then
            DeleteSelection;
          InsertAsRTF(mstream.DataString);

          didRTF := true;
        finally
          mstream.Free;
        end;
      finally
        GlobalUnlock(HClip);
      end;
    end;
    {$ENDIF}
    Clipboard.Close;

    if didRTF then
      Exit;
  end;


  if Clipboard.HasFormat(CF_BITMAP) and (cfBMP in ClipboardFormats) then
  begin
    Clipboard.Open;
    bmp := Graphics.TBitmap.Create;
    bmp.Assign(Clipboard);
    Clipboard.Close;

    pic := TPictureElement.Create;
    pic.Picture.Assign(bmp);
    bmp.Free;

    if not pic.Picture.Empty then
      PushContext;

    Clipboard.Close;

    pic.FitToPage(Width,Height);

    InsertElementAtCaret(pic);
    Exit;
  end;


  if Clipboard.HasFormat(ClipboardHTMLFMT) and (cfHTML in ClipboardFormats) then
  begin
    Clipboard.Open;

    {$IFNDEF LCLLIB}
    HClip := Clipboard.GetAsHandle(ClipboardHTMLFMT);

    bufptr := GlobalLock(HClip);
    if bufptr <> nil then
    begin
      try
        mstream := TStringStream.Create('');
        try
          mstream.WriteBuffer(bufptr^, GlobalSize(HClip));

          htmlvalue := UTF8ToString(RawByteString(mstream.DataString));
          c := pos('<HTML', Uppercase(htmlvalue));

          if c > 0 then
          begin
            didHTML := true;
            delete(htmlvalue, 1, c - 1);
            ParseHTML(htmlvalue);
          end;
        finally
          mstream.Free;
        end;
      finally
        GlobalUnlock(HClip);
      end;
    end;
    {$ENDIF}

    Clipboard.Close;
    if didHTML then
      Exit;
  end;

  if (Clipboard.HasFormat(CF_TEXT) or Clipboard.HasFormat(CF_UNICODETEXT)) and (cfText in ClipboardFormats) then
  begin
    Clipboard.Open;
    Result := Clipboard.AsText;
    Clipboard.Close;

    Allow := true;

    DoPasteText(Result, Allow);

    if Allow then
    begin
      if Result <> '' then
        PushContext;

      DeleteSelection;
      InsertMultiLineText(Result);
    end;
    Exit;
  end;

  if Clipboard.HasFormat(CF_HDROP) and (cfFile in ClipboardFormats) then
  begin
    Clipboard.Open;
    {$IFNDEF LCLLIB}
    hDrop := Clipboard.GetAsHandle(CF_HDROP);
    if hDrop <> 0 then
    begin
      count := DragQueryFile(hDrop, UINT(-1), nil, 0);

      for c := 0 to count - 1 do
      begin
        SetLength(AFileName, MAX_PATH);
        ln := DragQueryFile(hDrop, c, PChar(AFileName), MAX_PATH);
        SetLength(AFileName,ln);

        AExt := Uppercase(ExtractFileExt(AFileName));

        if (AExt = '.PNG') or (AExt = '.GIF') or (AExt = '.BMP') or (AExt = '.ICO') or (AExt = '.JPG') or (AExt = '.JPEG') then
        begin
          pic := TPictureElement.Create;
          pic.Picture.LoadFromFile(AFileName);
          InsertElementAtCaret(pic);
        end
        else
        begin
          sl := TStringList.Create;
          sl.LoadFromFile(AFileName);
          try
            InsertMultiLineText(sl.Text);
          finally
            sl.Free;
          end;
        end;
      end;
    end;
    {$ENDIF}
    Clipboard.Close;
  end;

  //if Clipboard.HasFormat(CF_PICTURE) then
  //begin
  //
  //end;
end;

procedure TAdvRichEditor.Print;
var
  FElementIndex,WordIndex,ToElementIndex,ToWordIndex,Descent,BaseLine,Indent: integer;
  FAlign: TAlignment;
  LW,LH,x,y: integer;
  MaxLineWidth: integer;
  MaxPageHeight: integer;
  margin: integer;
  dpi: double;
  Sel: TSelection;

begin
  Printer.BeginDoc;

  FDoCaret := false;
  FCaretTimer.Enabled := false;

  Sel := TSelection.Create;
  Sel.Assign(Selection);

  Selection.FromElement := nil;
  Selection.ToElement := nil;

  HideSelection := true;

  FElementIndex := 0;
  WordIndex := 0;
  Indent := 0;
  BaseLine := 0;

  {$IFNDEF LCLLIB}
  dpi := GetDeviceCaps(Printer.Handle,LOGPIXELSX);
  {$ENDIF}
  {$IFDEF LCLLIB}
  dpi := 96;
  {$ENDIF}

  DPIRatio := dpi / 96;

  MaxLineWidth := Printer.Canvas.ClipRect.Right - Printer.Canvas.ClipRect.Left;
  MaxPageHeight := Printer.Canvas.ClipRect.Bottom - Printer.Canvas.ClipRect.Top;

  margin := MaxLineWidth div 20;

  MaxLineWidth := MaxLineWidth - 2 * margin;
  MaxPageHeight := MaxPageHeight - 2 * margin;

  y := margin;

  while (FElementIndex < Context.Content.Count) do
  begin
    CalcLine(Printer.Canvas, -1, FElementIndex, WordIndex, MaxLineWidth, LW, LH, ToElementIndex, ToWordIndex, Descent, BaseLine, Indent, FAlign);

    x := OffsetX + Indent + Printer.Canvas.ClipRect.Left + margin;

    case FAlign of
      taRightJustify: x := margin + OffsetX + Round(MaxLineWidth - LW - 1);
      taCenter: x := margin + OffsetX + Round(MaxLineWidth - LW - 1) div 2;
    end;

    if Caret.NextLine then
    begin
      Caret.XY := Point(Caret.XY.X, y);
      Caret.LH := LH;
      Caret.NextLine := false;
    end;

    DrawLine(Printer.Canvas, x,y, FElementIndex, WordIndex, MaxLineWidth, LW, LH, Descent, BaseLine, True);

    y := y + LH;

    if (y > MaxPageHeight) and Clip then
    begin
      Printer.NewPage;
      y := margin;
    end;

    FElementIndex := ToElementIndex;
    WordIndex := ToWordIndex;
  end;

  Printer.EndDoc;

  Selection.Assign(Sel);
  Sel.Free;

  HideSelection := false;
  DPIRatio := 1.0;

  Refresh;

  FDoCaret := true;
  FCaretTimer.Enabled := true;
end;

procedure TAdvRichEditor.Refresh;
begin
  FDoCaret := true;
  Invalidate;
end;

procedure TAdvRichEditor.Resize;
begin
  inherited;
  UpdateSize;
  Refresh;
end;

procedure TAdvRichEditor.SetZoomFactor(const Value: Double);
begin
  FZoomFactor := Value;
  UpdateSize;
  Refresh;
end;

procedure TAdvRichEditor.SetGraphicSelection(const Value: TGraphicSelection);
begin
  FGraphicSelection.Assign(Value);
end;

procedure TAdvRichEditor.SetVersion(const Value: string);
begin
  // readonly
end;

procedure TAdvRichEditor.UpdateSelection;
begin
  //
end;

procedure TAdvRichEditor.WMDestroy(var Message: TWMDestroy);
begin
  inherited;
  {$IFNDEF LCLLIB}
  if FDropTargetAssigned then
  begin
    FDropTargetAssigned := false;
    RevokeDragDrop(Handle);
  end;
  {$ENDIF}
end;

procedure TAdvRichEditor.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS + DLGC_WANTCHARS;

  if WantTab then
    Message.Result := Message.Result + DLGC_WANTTAB;
end;

procedure TAdvRichEditor.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;

  if FAllowSelect then
  begin
    SelectWordAtXY(Message.XPos, Message.YPos);
    FClickOnSel := false;
    FMouseDown := false;
    Update;
  end;

//  ShowTree;
end;

{ TGraphicSelection }

procedure TGraphicSelection.Assign(Source: TPersistent);
begin
  if (Source is TGraphicSelection) then
  begin
    FColor := (Source as TGraphicSelection).Color;
    FBorderColor := (Source as TGraphicSelection).BorderColor;
    Style := (Source as TGraphicSelection).Style;
  end;
end;

constructor TGraphicSelection.Create;
begin
  inherited;
  FColor := clWhite;
  FBorderColor := clGray;
  FStyle := gsRect;
end;

{$IFNDEF LCLLIB}

{ TAdvRichEditorDropTarget }

constructor TAdvRichEditorDropTarget.Create(AEditor: TAdvRichEditor);
begin
  inherited Create;

  FRichEditor := AEditor;
end;

procedure TAdvRichEditorDropTarget.DragMouseLeave;
begin
  inherited;
end;

procedure TAdvRichEditorDropTarget.DragMouseMove(pt: TPoint; var Allow: Boolean;
  DropFormats: TDropFormats);
var
  IsSel: boolean;
  sp: TPoint;
begin
  pt := FRichEditor.ScreenToClient(pt);

  sp := FRichEditor.TopLeft;

  if (pt.y < 10) and (sp.Y > 0) then
  begin
    FRichEditor.TopLeft := Point(sp.X, sp.Y - FRichEditor.Caret.LH);
    Exit;
  end;

  if (pt.y > FRichEditor.Height - 10) then
  begin
    FRichEditor.TopLeft := Point(sp.X, sp.Y + FRichEditor.Caret.LH);
    Exit;
  end;

  FRichEditor.PushCaret;
  FRichEditor.PushSelection;

  try
    FRichEditor.XYToCaret(pt.X, pt.Y, False, IsSel);
    FRichEditor.Caret.XY := Point(-1,-1);
    FRichEditor.CalcCaretXY;
    FRichEditor.DragCaret.Assign(FRichEditor.Caret);
  finally
    FRichEditor.PopSelection;
    FRichEditor.PopCaret;
  end;
end;

procedure TAdvRichEditorDropTarget.DropBMP(pt: TPoint; bmp: Graphics.TBitmap; dwEffect: longint);
var
  IsSel: boolean;
begin
  if FRichEditor.FInternalDD and (dwEffect = DROPEFFECT_MOVE) then
  begin
    FRichEditor.DeleteSelection;
  end;

  FRichEditor.FInternalDD := false;

  pt := FRichEditor.ScreenToClient(pt);

  FRichEditor.XYToCaret(pt.X, pt.Y, False, IsSel);
  FRichEditor.InsertImage(TPicture(bmp),0,0);
end;

procedure TAdvRichEditorDropTarget.DropFiles(pt: TPoint; Files: TStrings; dwEffect: longint);
var
  fn,ext: string;
  IsSel: boolean;
  mStream: TStringStream;
begin
  inherited;

  if (Files.Count > 0) then
  begin
    fn := Files[0];

    ext := Uppercase(ExtractFileExt(fn));

    if (Ext = '.BMP') or (Ext = '.JPEG') or (Ext = '.JPG') or  (Ext = '.GIF') or (Ext = '.ICO') or (Ext = '.PNG') then
    begin
      pt := FRichEditor.ScreenToClient(pt);
      FRichEditor.XYToCaret(pt.X, pt.Y,False,IsSel);
      FRichEditor.InsertImage(fn,0,0);
      FRichEditor.SetFocus;
    end
    else
    if (Ext = '.RTE') then
      FRichEditor.LoadFromFile(fn)
    else
    if (Ext = '.TXT') then
      FRichEditor.LoadFromTextFile(fn)
    else
    if (Ext = '.RTF') then
    begin
      mStream := TStringStream.Create('');
      mStream.LoadFromFile(fn);
      try
        FRichEditor.InsertAsRTF(mstream.DataString);
      finally
        mStream.Free;
      end;
    end;
  end;
end;

procedure TAdvRichEditorDropTarget.DropRTF(pt: TPoint; s: string; dwEffect: longint);
begin
  inherited;
end;

procedure TAdvRichEditorDropTarget.DropStream(pt: TPoint; AStream: TMemoryStream; dwEffect: longint);
var
  mss: TStateSaver;
  ms: TMemoState;
  isSel: boolean;
begin
  if FRichEditor.FInternalDD and (dwEffect = DROPEFFECT_MOVE) then
    FRichEditor.DeleteSelection;

  FRichEditor.FInternalDD := false;

  FRichEditor.RegisterClasses;

  // read memo state first
  mss := TStateSaver.Create(nil);
  ms := TMemoState.Create;
  mss.SaveState := ms;
  try
    AStream.ReadComponent(mss);
    FRichEditor.LoadMemoState(ms, True);
  finally
    mss.Free;
    ms.Free;
  end;

  pt := FRichEditor.ScreenToClient(pt);
  FRichEditor.XYToCaret(pt.X, pt.Y, False, IsSel);

  FRichEditor.InsertFromStream(AStream, 1);
end;

procedure TAdvRichEditorDropTarget.DropText(pt: TPoint; s: string; dwEffect: longint);
var
  el: TREElement;
  SP: TPoint;
  hasElem, IsSel: boolean;
  t: string;
  idx, cx, cy: integer;
begin
  inherited;

  if FRichEditor.FInternalDD and (dwEffect = DROPEFFECT_MOVE) then
    FRichEditor.DeleteSelection;

  FRichEditor.FInternalDD := false;

  pt := FRichEditor.ScreenToClient(pt);
  SP := FRichEditor.TopLeft;

  if pt.x < FRichEditor.PageMargin.Horizontal then
    pt.x := FRichEditor.PageMargin.Horizontal + 3;
  if pt.y < FRichEditor.PageMargin.Vertical then
    pt.y := FRichEditor.PageMargin.Vertical + 3;
  if pt.x > FRichEditor.Width - FRichEditor.PageMargin.Horizontal then
    pt.x := FRichEditor.Width - FRichEditor.PageMargin.Horizontal;
  if pt.y > FRichEditor.Height then
    pt.y := FRichEditor.Height;

  hasElem := FRichEditor.XYToElement(pt.X + sp.X,pt.Y + sp.Y, el);

  if hasElem and (el is TTextElement) then
  begin
    t := (el as TTextElement).DisplayText;
    idx := FRichEditor.XYToChar(pt.X + SP.X,pt.Y + sp.Y,el, CX, CY);

    if idx > -1 then
    begin
      FRichEditor.Selection.ToElement := el;
      FRichEditor.Selection.ToChar := idx;
      FRichEditor.Caret.Element := el;
      FRichEditor.Caret.CharIndex := idx;
      FRichEditor.InsertText(s);
    end;
  end
  else
  begin
    pt := FRichEditor.ScreenToClient(pt);
    FRichEditor.XYToCaret(pt.X, pt.Y,False,IsSel);
    FRichEditor.InsertText(s);
  end;

  FRichEditor.DragCaret.Element := nil;
  FRichEditor.Update;
end;

procedure TAdvRichEditorDropTarget.DropURL(pt: TPoint; s: string; dwEffect: longint);
var
  el: TREElement;
  IsSel: boolean;
begin
  pt := FRichEditor.ScreenToClient(pt);
  FRichEditor.XYToCaret(pt.X, pt.Y,False,IsSel);

  el := FRichEditor.InsertText(s);
  el.URL := s;
end;

{ TAdvRichEditorDropSource }

constructor TAdvRichEditorDropSource.Create(AEditor: TAdvRichEditor);
begin
  inherited Create;
  FRichEditor := AEditor;
end;

procedure TAdvRichEditorDropSource.CurrentEffect(dwEffect: Integer);
begin
  if dwEffect = DROPEFFECT_MOVE then
    FLastEffect := dwEffect;

  if dwEffect = DROPEFFECT_COPY then
    FLastEffect := dwEffect;
end;

procedure TAdvRichEditorDropSource.DragDropStop;
begin
  inherited;
end;

procedure TAdvRichEditorDropSource.QueryDrag;
begin
  inherited;
end;
{$ENDIF}

{ TAdvRichEditorPopup }

procedure TAdvRichEditorPopup.Hide;
begin
  FVisible := false;
end;

procedure TAdvRichEditorPopup.Show(PT: TPoint);
begin
  FVisible := true;
end;


end.
