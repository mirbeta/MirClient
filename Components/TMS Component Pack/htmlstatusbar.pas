{*************************************************************************}
{ THTMLStatusBar component                                                }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2000 - 2013                                       }
{           Email : info@tmssoftware.com                                  }
{           Website : http://www.tmssoftware.com/                         }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit HTMLStatusBar;

{$I TMSDEFS.INC}
{$DEFINE REMOVESTRIP}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, CommCtrl, ImgList, ComObj, ShellAPI, PictureContainer, StdActns
  , Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;
const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 5; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.3.0.0 : Added support for PictureContainer
  //         : Added HTML property editor for SimpleText & Panel.Text
  // 1.3.0.1 : Improved progress bar drawing
  // 1.3.0.2 : Fixed background painting color
  // 1.3.0.3 : Fixed painting issue with resizing and sizegrip
  // 1.3.0.4 : Fixed sizegrip redrawing in Delphi 2009

  // 1.4.0.0 : Improved DFM property persistence
  // 1.4.1.0 : New : Name & Tag property added to THTMLStatusPanel
  // 1.4.2.0 : Improved : right alignment of text in last panel with sizegrip
  //         : New : psDateTime panel style added
  // 1.4.3.0 : New : support for customizing bullets in HTML UL lists
  // 1.4.3.1 : Fixed : Issue with use of OnDrawPanel event
  // 1.5.0.0 : New : Support for PNG images via images in associated PictureContainer


type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

{ THTMLStatusBar }

  THTMLStatusBar = class;

  THTMLStatusPanelStyle = (psHTML, psText, psOwnerDraw, psTime, psDate, psNumLock, psCapsLock, psScrollLock, psProgress, psImage, psImageList, psAnimation, psEllipsText, psFileEllipsText, psDateTime);
  THTMLStatusPanelBevel = (pbNone, pbLowered, pbRaised);
  TGaugeOrientation = (goHorizontal, goVertical);

  TAnchorClick = procedure(Sender: TObject; Anchor: string) of object;

  TPanelClick = procedure(Sender: TObject; PanelIndex: Integer) of object;

  TProgressIndication = (piPercentage, piAbsolute, piNone);

  THTMLStatusPanel = class;

  TProgressStyle = class(TPersistent)
  private
    FMin: integer;
    FPosition: integer;
    FMax: integer;
    FIndication: TProgressIndication;
    FBackground: TColor;
    FTextColor: TColor;
    FOwner: THTMLStatusPanel;
    FLevel0Color: TColor;
    FLevel0ColorTo: TColor;
    FLevel1Color: TColor;
    FLevel1ColorTo: TColor;
    FLevel2Color: TColor;
    FLevel2ColorTo: TColor;
    FLevel3Color: TColor;
    FLevel3ColorTo: TColor;
    FLevel1Perc: Integer;
    FLevel2Perc: Integer;
    FBorderColor: TColor;
    FShowBorder: Boolean;
    FStacked: Boolean;
    FShowPercentage: Boolean;
    FCompletionSmooth: Boolean;
    FShowGradient: Boolean;
    FSuffix: string;
    FPrefix: string;
    procedure SetIndication(const Value: TProgressIndication);
    procedure SetMax(const Value: integer);
    procedure SetMin(const Value: integer);
    procedure SetPosition(const Value: integer);
    procedure SetBackGround(const Value: tColor);
    procedure SetTextColor(const Value: tColor);
    procedure SetLevel0Color(const Value: tColor);
    procedure SetLevel0ColorTo(const Value: tColor);
    procedure SetLevel1Color(const Value: tColor);
    procedure SetLevel1ColorTo(const Value: tColor);
    procedure SetLevel2Color(const Value: tColor);
    procedure SetLevel2ColorTo(const Value: tColor);
    procedure SetLevel3Color(const Value: tColor);
    procedure SetLevel3ColorTo(const Value: tColor);
    procedure SetLevel1Perc(Value: integer);
    procedure SetLevel2Perc(Value: integer);
    procedure SetBorderColor(const Value: tColor);
    procedure SetShowBorder(Value: boolean);
    procedure SetStacked(Value: boolean);
    procedure SetShowPercentage(Value: boolean);
    procedure SetCompletionSmooth(Value: boolean);
    procedure SetShowGradient(Value: boolean);
    procedure SetPrefix(const Value: string);
    procedure SetSuffix(const Value: string);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: THTMLStatusPanel);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property BackGround: TColor read FBackground write SetBackGround;
    property TextColor: TColor read FTextColor write SetTextColor;
    property Indication: TProgressIndication read FIndication write SetIndication;
    property Min: integer read FMin write SetMin;
    property Max: integer read FMax write SetMax;
    property Position: integer read FPosition write SetPosition;

    property Level0Color: TColor read FLevel0Color write SetLevel0Color;
    property Level0ColorTo: TColor read FLevel0ColorTo write SetLevel0ColorTo;
    property Level1Color: TColor read FLevel1Color write SetLevel1Color;
    property Level1ColorTo: TColor read FLevel1ColorTo write SetLevel1ColorTo;
    property Level2Color: TColor read FLevel2Color write SetLevel2Color;
    property Level2ColorTo: TColor read FLevel2ColorTo write SetLevel2ColorTo;
    property Level3Color: TColor read FLevel3Color write SetLevel3Color;
    property Level3ColorTo: TColor read FLevel3ColorTo write SetLevel3ColorTo;
    property Level1Perc: Integer read FLevel1Perc write SetLevel1Perc;
    property Level2Perc: Integer read FLevel2Perc write SetLevel2Perc;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property ShowBorder: Boolean read FShowBorder write SetShowBorder;
    property Stacked: Boolean read FStacked write SetStacked;
    property ShowPercentage: Boolean read FShowPercentage write SetShowPercentage default True;
    property CompletionSmooth: Boolean read FCompletionSmooth write SetCompletionSmooth default False;
    property Suffix: string read FSuffix write SetSuffix;
    property Prefix: string read FPrefix write SetPrefix;
    property ShowGradient: Boolean read FShowGradient write SetShowGradient default true;
  end;

  THTMLStatusPanel = class(TCollectionItem)
  private
    FText: string;
    FWidth: Integer;
    FAlignment: TAlignment;
    FBevel: THTMLStatusPanelBevel;
    FBiDiMode: TBiDiMode;
    FParentBiDiMode: Boolean;
    FStyle: THTMLStatusPanelStyle;
    FUpdateNeeded: Boolean;
    FTimeFormat: string;
    FDateFormat: string;
    FHint: string;
    FAnimIndex: Integer;
    FProgressStyle: TProgressStyle;
    FHTMLOffsetY: Integer;
    FHTMLOffsetX: Integer;
    FImageIndex: Integer;
    FImageIndexes: TStringList;
    FAnimationImages: TImageList;
    FAnimationDelay: Integer;
    FAnimated: Boolean;
    FEnabled: Boolean;
    FTag: integer;
    FName: string;
    procedure SetAlignment(Value: TAlignment);
    procedure SetBevel(Value: THTMLStatusPanelBevel);
    procedure SetBiDiMode(Value: TBiDiMode);
    procedure SetParentBiDiMode(Value: Boolean);
    function IsBiDiModeStored: Boolean;
    procedure SetStyle(Value: THTMLStatusPanelStyle);
    procedure SetText(const Value: string);
    procedure SetWidth(Value: Integer);
    procedure SetDateFormat(const Value: string);
    procedure SetTimeFormat(const Value: string);
    procedure SetProgressStyle(const Value: TProgressStyle);
    procedure SetHint(const Value: string);
    procedure SetHTMLOffsetX(const Value: Integer);
    procedure SetHTMLOffseTY(const Value: Integer);
    procedure SetImageIndex(const Value: Integer);
    function GetImageIndexes(Index: Integer): Integer;
    procedure SetImageIndexes(Index: Integer; const Value: Integer);
    procedure SetAnimationImages(const Value: TImageList);
    procedure SetAnimated(const Value: Boolean);
    procedure SetAnimationDelay(const Value: Integer);
    procedure SetEnabled(const Value: Boolean);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure ParentBiDiModeChanged;
    function UseRightToLeftAlignment: Boolean;
    function UseRightToLeftReading: Boolean;
    procedure ClearImageIndexes;
    function ImageCount: Integer;
    property ImageIndexes[Index: Integer]: Integer read GetImageIndexes write SetImageIndexes;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Animated: Boolean read FAnimated write SetAnimated default false;
    property AnimationImages: TImageList read FAnimationImages write SetAnimationImages;
    property AnimationDelay: Integer read FAnimationDelay write SetAnimationDelay default 0;

    property Bevel: THTMLStatusPanelBevel read FBevel write SetBevel default pbLowered;
    property BiDiMode: TBiDiMode read FBiDiMode write SetBiDiMode stored IsBiDiModeStored;
    property ParentBiDiMode: Boolean read FParentBiDiMode write SetParentBiDiMode default True;
    property DateFormat: string read FDateFormat write SetDateFormat;
    property Enabled: Boolean read FEnabled write SetEnabled default true;
    property Hint: string read FHint write SetHint;
    property HTMLOffsetX: Integer read FHTMLOffsetX write SetHTMLOffsetX default 2;
    property HTMLOffsetY: Integer read FHTMLOffsetY write SetHTMLOffseTY default 2;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Name: string read FName write FName;
    property Progress: TProgressStyle read fProgressStyle write SetProgressStyle;
    property Style: THTMLStatusPanelStyle read FStyle write SetStyle default psHTML;
    property Text: string read FText write SetText;
    property Tag: integer read FTag write FTag default 0;
    property TimeFormat: string read FTimeFormat write SetTimeFormat;
    property Width: Integer read FWidth write SetWidth;
  end;

  THTMLStatusPanels = class(TCollection)
  private
    FStatusBar: THTMLStatusBar;
    function GetItem(Index: Integer): THTMLStatusPanel;
    procedure SetItem(Index: Integer; Value: THTMLStatusPanel);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(StatusBar: THTMLStatusBar);
    function Add: THTMLStatusPanel;
    property Items[Index: Integer]: THTMLStatusPanel read GetItem write SetItem; default;
  end;

  TDrawPanelEvent = procedure(StatusBar: THTMLStatusBar; Panel: THTMLStatusPanel;
    const Rect: TRect) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  THTMLStatusBar = class(TWinControl)
  private
    FPanels: THTMLStatusPanels;
    FCanvas: TCanvas;
    FSimpleText: string;
    FSimplePanel: Boolean;
    FSizeGrip: Boolean;
    FUseSystemFont: Boolean;
    FAutoHint: Boolean;
    FOnDrawPanel: TDrawPanelEvent;
    FOnHint: TNotifyEvent;
    FURLColor: TColor;
    FTimerID: Integer;
    FTimerCount: Integer;
    FImages: TImageList;
    FMousePanel: integer;
    FAnchor: string;
    FAnchorHint: boolean;
    FAnchorClick: TAnchorClick;
    FAnchorEnter: TAnchorClick;
    FAnchorExit: TAnchorClick;
    FOnPanelClick: TPanelClick;
    FOnPanelDblClick: TPanelClick;
    FOnPanelRightClick: TPanelClick;
    FContainer: TPictureContainer;
    function IsAnchor(x, y: integer): string;
    function GetPanel(x: integer): integer;
    procedure DoRightToLeftAlignment(var Str: string; AAlignment: TAlignment;
      ARTLAlignment: Boolean);
    function IsFontStored: Boolean;
    procedure SetPanels(Value: THTMLStatusPanels);
    procedure SetSimplePanel(Value: Boolean);
    procedure UpdateSimpleText;
    procedure SetSimpleText(const Value: string);
    procedure SetSizeGrip(Value: Boolean);
    procedure SyncToSystemFont;
    procedure UpdatePanel(Index: Integer; Repaint: Boolean);
    procedure UpdatePanels(UpdateRects, UpdateText: Boolean);
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMWinIniChange(var Message: TMessage); message CM_WININICHANGE;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMGetTextLength(var Message: TWMGetTextLength); message WM_GETTEXTLENGTH;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure SetUseSystemFont(const Value: Boolean);
    procedure SetImages(const Value: TImageList);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
  protected
    procedure UpdateStatusBar; virtual;
    procedure ChangeScale(M, D: Integer); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Loaded; override;
    procedure Resize; override;
    function DoHint: Boolean; virtual;
    procedure DrawPanel(Panel: THTMLStatusPanel; const Rect: TRect); dynamic;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure FlipChildren(AllLevels: Boolean); override;
    property Canvas: TCanvas read FCanvas;
    property PanelIndex[x: integer]: Integer read GetPanel;
  published
    property Action;
    property Anchors;
    property BiDiMode;
    property BorderWidth;
    property DragKind;
    property Constraints;
    property ParentBiDiMode;
    property AnchorHint: boolean read fAnchorHint write fAnchorHint;
    property AutoHint: Boolean read FAutoHint write FAutoHint default False;
    property Align default alBottom;
    property Color default clBtnFace;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font stored IsFontStored;
    property Images: TImageList read fImages write SetImages;
    property Panels: THTMLStatusPanels read FPanels write SetPanels;
    property ParentColor default False;
    property ParentFont default False;
    property ParentShowHint;
    property PictureContainer: TPictureContainer read FContainer write FContainer;
    property PopupMenu;
    property ShowHint;
    property SimplePanel: Boolean read FSimplePanel write SetSimplePanel;
    property SimpleText: string read FSimpleText write SetSimpleText;
    property SizeGrip: Boolean read FSizeGrip write SetSizeGrip default True;
    property URLColor: TColor read fURLColor write fURLColor;
    property UseSystemFont: Boolean read FUseSystemFont write SetUseSystemFont default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnStartDock;
    property OnResize;
    property OnEndDrag;
    property OnHint: TNotifyEvent read FOnHint write FOnHint;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDrawPanel: TDrawPanelEvent read FOnDrawPanel write FOnDrawPanel;
    property OnPanelClick: TPanelClick read FOnPanelClick write FOnPanelClick;
    property OnPanelRightClick: TPanelClick read FOnPanelRightClick write FOnPanelRightClick;
    property OnPanelDblClick: TPanelClick read FOnPanelDblClick write FOnPanelDblClick;
    property OnStartDrag;
    property OnAnchorClick: TAnchorClick read fAnchorClick write fAnchorClick;
    property OnAnchorEnter: TAnchorClick read fAnchorEnter write fAnchorEnter;
    property OnAnchorExit: TAnchorClick read fAnchorExit write fAnchorExit;
    property Version: string read GetVersion write SetVersion;
  end;

  TGaugeSettings = record
    Level0Color: TColor;
    Level0ColorTo: TColor;
    Level1Color: TColor;
    Level1ColorTo: TColor;
    Level2Color: TColor;
    Level2ColorTo: TColor;
    Level3Color: TColor;
    Level3ColorTo: TColor;
    Level1Perc: Integer;
    Level2Perc: Integer;
    BorderColor: TColor;
    ShowBorder: Boolean;
    Stacked: Boolean;
    ShowPercentage: Boolean;
    Font: TFont;
    CompletionSmooth: Boolean;
    ShowGradient: Boolean;
    Steps: Integer;
    Position: Integer;
    BackgroundColor: TColor;
    Orientation: TGaugeOrientation;
    IsPercent: Boolean;
    Suffix: string;
    Prefix: string;
  end;


procedure DrawRectangle(Canvas: TCanvas; R: TRect; aColor: TColor);
procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
procedure DrawGauge(Canvas: TCanvas; R: TRect; Position: Integer; Settings: TGaugeSettings);
procedure DivideInSegment(Canvas: TCanvas; R: TRect; Position: integer);

implementation


const
  CAPSLOCK = 'CAP';
  NUMLOCK = 'NUM';
  SCROLLLOCK = 'SCRL';

{$I HtmlEngo.pas}

procedure DrawRectangle(Canvas: TCanvas; R: TRect; aColor: TColor);
begin
  canvas.Brush.Color := aColor;
  Canvas.FillRect(R);
end;

procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  rstepr, rstepg, rstepb, rstepw: Real;
  i, stepw: Word;

begin
  if Steps = 0 then
    Steps := 1;

  FromColor := ColorToRGB(FromColor);
  ToColor := ColorToRGB(ToColor);

  startr := (FromColor and $0000FF);
  startg := (FromColor and $00FF00) shr 8;
  startb := (FromColor and $FF0000) shr 16;
  endr := (ToColor and $0000FF);
  endg := (ToColor and $00FF00) shr 8;
  endb := (ToColor and $FF0000) shr 16;

  diffr := endr - startr;
  diffg := endg - startg;
  diffb := endb - startb;

  rstepr := diffr / steps;
  rstepg := diffg / steps;
  rstepb := diffb / steps;

  if Direction then
    rstepw := (R.Right - R.Left) / Steps
  else
    rstepw := (R.Bottom - R.Top) / Steps;

  with Canvas do
  begin
    for i := 0 to steps - 1 do
    begin
      endr := startr + Round(rstepr * i);
      endg := startg + Round(rstepg * i);
      endb := startb + Round(rstepb * i);
      stepw := Round(i * rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
        Rectangle(R.Left + stepw, R.Top, R.Left + stepw + Round(rstepw) + 1, R.Bottom)
      else
        Rectangle(R.Left, R.Top + stepw, R.Right, R.Top + stepw + Round(rstepw) + 1);
    end;
  end;
end;

procedure DivideInSegment(Canvas: TCanvas; R: TRect; Position: integer);
var
  i: integer;
  r1: TRect;
begin
  r1.Top := r.Top;
  r1.Left := r.Left + 8;
  r1.Right := r.Left + 10;
  r1.Bottom := R.Bottom;

  for i := 0 to (R.Right div 9) do
  begin
    if (r1.Right < r.Right) then
      Canvas.FillRect(r1);

    r1.Left := r1.Left + 9;
    r1.Right := r1.Left + 2;
  end;

end;

procedure DrawGauge(Canvas: TCanvas; R: TRect; Position: Integer;
  Settings: TGaugeSettings);
var
  RectL: TRect;
  RectM: TRect;
  RectR: TRect;

  WidthBar: integer;
  WidthPart: Integer;
  Continue: Boolean;
  GradDir: Boolean;
  BrushColor: TColor;
  BrushColorTo: TColor;
  Percentage: Integer;
  BarFilled: Integer;
  s: string;

{$IFNDEF TMSCLX}
  lf: TLogFont;
{$ENDIF}
  tf: TFont;

  R1: TRect;
  R2: TRect;
begin
  WidthBar := R.Right - R.Left;

  Continue := true;
  Percentage := -1;
  Canvas.Brush.Color := Settings.BackgroundColor;
  GradDir := false;

  if (Settings.ShowPercentage) then
    Percentage := Position;

  //Draw Border
  if (Settings.ShowBorder) then
  begin
    Canvas.Pen.Color := Settings.BorderColor;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psSolid;
    InflateRect(R, -1, -1);
  end
  else
    Canvas.Pen.Color := Settings.BackgroundColor;

  Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);

  WidthBar := WidthBar - 2;

  if Settings.ShowBorder then
    WidthBar := WidthBar - 2;

  if (Position > 0) then
  begin
    // stacked display

    if Settings.Stacked then
    begin
      if (Position >= Settings.Level1Perc) then
        WidthPart := Round((Settings.Level1Perc / 100) * WidthBar)
      else
      begin
        WidthPart := Round((Position / 100) * WidthBar);
        Continue := false;
      end;

      //Draw first part
      RectL.Left := R.Left + 1;
      RectL.Top := R.Top + 1;
      RectL.Right := RectL.Left + WidthPart;
      RectL.Bottom := r.Bottom - 2;

      if Settings.ShowGradient then
      begin
        R1.Left := RectL.Left;
        R1.Right := RectL.Right;
        R1.Top := RectL.Top;
        R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
        R2.Top := R1.Bottom;
        R2.Left := RectL.Left;
        R2.Right := RectL.Right;
        R2.Bottom := RectL.Bottom;

        DrawGradient(Canvas, Settings.Level0ColorTo, Settings.Level0Color, Settings.Steps, R1, GradDir);
        DrawGradient(Canvas, Settings.Level0Color, Settings.Level0ColorTo, Settings.Steps, R2, GradDir);
      end
      else
        DrawRectangle(Canvas, RectL, Settings.Level0Color);

      BarFilled := WidthPart;

      if not Settings.CompletionSmooth then
      begin
        RectL.Top := RectL.Top + 1;
        RectL.Bottom := RectL.Bottom - 1;
        Canvas.Brush.Color := Settings.BackgroundColor;
        DivideInSegment(Canvas, RectL, Position);
      end;

      if (Continue) then
      begin
        //Draw second part
        RectM.Left := RectL.Right;
        RectM.Top := r.Top + 1;
        RectM.Bottom := r.Bottom - 2;

        if (Position >= Settings.Level2Perc) then
          WidthPart := Round(WidthBar * ((Settings.Level2Perc - Settings.Level1Perc) / 100))
        else
        begin
          WidthPart := Round(WidthBar * ((Position - Settings.Level1Perc) / 100));
          Continue := false;
        end;

        RectM.Right := WidthPart + RectM.Left;

        if (Settings.ShowGradient) then
        begin
          R1.Left := RectM.Left;
          R1.Right := RectM.Right;
          R1.Top := RectM.Top;
          R1.Bottom := RectM.Top + (RectM.Bottom - RectM.Top) div 2;
          R2.Top := R1.Bottom;
          R2.Left := RectM.Left;
          R2.Right := RectM.Right;
          R2.Bottom := RectM.Bottom;

          DrawGradient(Canvas, Settings.Level1ColorTo, Settings.Level1Color, Settings.Steps, R1, GradDir);
          DrawGradient(Canvas, Settings.Level1Color, Settings.Level1ColorTo, Settings.Steps, R2, GradDir);
        end
        else
          DrawRectangle(Canvas, RectM, Settings.Level1Color);

        if not Settings.CompletionSmooth then
        begin
          RectM.Top := RectM.Top + 1;
          RectM.Bottom := RectM.Bottom - 1;
          Canvas.Brush.Color := Settings.BackgroundColor;
          DivideInSegment(Canvas, RectM, Position);
        end;

        BarFilled := BarFilled + WidthPart;
        if (Continue) then
        begin
          //Draw third part
          if (Position = 100) then
            WidthPart := Round(WidthBar - BarFilled)
          else
            WidthPart := Round(WidthBar * ((Position - Settings.Level2Perc) / 100));

          RectR.Left := RectM.Right;
          RectR.Top := R.Top + 1;
          RectR.Bottom := r.Bottom - 2;
          RectR.Right := RectR.Left + WidthPart;

          if (Settings.ShowGradient) then
          begin
            R1.Left := RectR.Left;
            R1.Right := RectR.Right;
            R1.Top := RectR.Top;
            R1.Bottom := RectR.Top + (RectR.Bottom - RectR.Top) div 2;
            R2.Top := R1.Bottom;
            R2.Left := RectR.Left;
            R2.Right := RectR.Right;
            R2.Bottom := RectR.Bottom;
            DrawGradient(Canvas, Settings.Level2ColorTo, Settings.Level2Color, Settings.Steps, R1, GradDir);
            DrawGradient(Canvas, Settings.Level2Color, Settings.Level2ColorTo, Settings.Steps, R2, GradDir);
          end
          else
            DrawRectangle(Canvas, RectR, Settings.Level3Color);

          if not Settings.CompletionSmooth then
          begin
            RectR.Top := RectR.Top + 1;
            RectR.Bottom := RectR.Bottom - 1;
            Canvas.Brush.Color := Settings.BackgroundColor;
            DivideInSegment(Canvas, RectR, Position);
          end;

        end;
      end;

    end
    else
    begin
      if (Position < Settings.Level1Perc) then
      begin
        BrushColor := Settings.Level0Color;
        BrushColorTo := Settings.Level0ColorTo;
      end
      else
      begin
        if (Position < Settings.Level2Perc) then
        begin
          BrushColor := Settings.Level1Color;
          BrushColorTo := Settings.Level1ColorTo;
        end
        else
        begin
          if (Position < 100) then
          begin
            BrushColor := Settings.Level2Color;
            BrushColorTo := Settings.Level2ColorTo;
          end
          else
          begin
            BrushColor := Settings.Level3Color;
            BrushColorTo := Settings.Level3ColorTo;
          end;
        end;
      end;

      if not (Settings.CompletionSmooth) then
      begin
        Canvas.Brush.Color := Settings.BackgroundColor;

        RectL.Left := R.Left + 2;
        RectL.Right := R.Left + Round((Position * WidthBar) / 100);
        RectL.Top := R.Top + 2;
        RectL.Bottom := R.Bottom - 2;

        if (Settings.ShowGradient) then
        begin
          R1.Left := RectL.Left;
          R1.Right := RectL.Right;
          R1.Top := RectL.Top;
          R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
          R2.Top := R1.Bottom;
          R2.Left := RectL.Left;
          R2.Right := RectL.Right;
          R2.Bottom := RectL.Bottom;
          DrawGradient(Canvas, BrushColorTo, BrushColor, Settings.Steps, R1, GradDir);
          DrawGradient(Canvas, BrushColor, BrushColorTo, Settings.Steps, R2, GradDir);
        end
        else
          DrawRectangle(Canvas, RectL, BrushColor);

        Canvas.Brush.Color := Settings.BackgroundColor;
        DivideInSegment(Canvas, RectL, Position);
      end
      else
      begin
        WidthPart := Round((Position / 100) * WidthBar);

        RectL.Left := R.Left + 1;
        RectL.Top := R.Top + 1;
        RectL.Right := RectL.Left + WidthPart;
        RectL.Bottom := R.Bottom - 1;

        if (Settings.ShowGradient) then
        begin
          if not (Settings.Orientation = goHorizontal) then
          begin
            R1.Left := RectL.Left;
            R1.Right := RectL.Left + (RectL.Right - RectL.Left) div 2;
            R1.Bottom := RectL.Bottom;
            R1.Top := RectL.Top;
            R2.Left := R1.Right;
            R2.Right := RectL.Right;
            R2.Bottom := RectL.Bottom;
            R2.Top := RectL.Top;
          end
          else
          begin
            R1.Left := RectL.Left;
            R1.Right := RectL.Right;
            R1.Top := RectL.Top;
            R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
            R2.Top := R1.Bottom;
            R2.Left := RectL.Left;
            R2.Right := RectL.Right;
            R2.Bottom := RectL.Bottom;
          end;
          DrawGradient(Canvas, BrushColorTo, BrushColor, Settings.Steps, R1, GradDir);
          DrawGradient(Canvas, BrushColor, BrushColorTo, Settings.Steps, R2, GradDir);
        end
        else
          DrawRectangle(Canvas, RectL, BrushColor);

      end;
    end;
  end;

  //Draw text with PositionPercentage
  if (Percentage <> -1) then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Name := Settings.Font.Name;
    Canvas.Font.Size := Settings.Font.Size;
    Canvas.Font.Color := Settings.Font.Color;
    Canvas.Font.Style := Settings.Font.Style;
    if not (Settings.Orientation = goHorizontal) then
    begin
      tf := TFont.Create;
      try
        tf.Assign(Settings.Font);

{$IFNDEF TMSCLX}

        GetObject(tf.Handle, sizeof(lf), @lf);

        lf.lfEscapement := 900;
        lf.lfOrientation := 900;
        tf.Handle := CreateFontIndirect(lf);
{$ENDIF}
        Canvas.Font.Assign(tf);
        Canvas.TextOut(R.Left + ((R.Right - R.Left) div 2 -
          (Canvas.TextHeight(IntToStr(Percentage) + '%') div 2)), R.Top +
          ((R.Bottom - R.Top) div 2) + Canvas.TextWidth(IntToStr(Percentage) + '%') div 2, IntToStr(Percentage) + '%');
      finally
        tf.Free;
      end;
    end
    else
    begin
      if Settings.IsPercent then
        s := IntToStr(Percentage) + '%'
      else
        s := Settings.Prefix + IntToStr(Settings.Position) + Settings.Suffix;

      Canvas.TextOut(((R.Right - R.Left) div 2) -
          (Canvas.TextWidth(s) div 2) + r.Left, r.Top +
          ((R.Bottom - R.Top) div 2) - Canvas.TextHeight(s) div 2, s);
    end;
  end;
end;

{ THTMLStatusPanel }

constructor THTMLStatusPanel.Create(Collection: TCollection);
begin
  FWidth := 50;
  FBevel := pbLowered;
  inherited Create(Collection);
  FParentBiDiMode := True;
  ParentBiDiModeChanged;
  FTimeFormat := 'hh:mm:ss';
  FDateFormat := 'mm/dd/yyyy';
  FProgressStyle := TProgressStyle.Create(self);
  FStyle := psHTML;
  FHTMLOffsetX := 2;
  FHTMLOffsetY := 2;
  FImageIndex := -1;
  FAnimIndex := 0;
  FAlignment := taLeftJustify;
  FEnabled := true;
  FImageIndexes := TStringList.Create;
  FName := '';
  FTag := 0;
end;

procedure THTMLStatusPanel.Assign(Source: TPersistent);
begin
  if Source is THTMLStatusPanel then
  begin
    Text := THTMLStatusPanel(Source).Text;
    Width := THTMLStatusPanel(Source).Width;
    Alignment := THTMLStatusPanel(Source).Alignment;
    Bevel := THTMLStatusPanel(Source).Bevel;
    Style := THTMLStatusPanel(Source).Style;
    DateFormat := THTMLStatusPanel(Source).DateFormat;
    TimeFormat := THTMLStatusPanel(Source).TimeFormat;
    Progress.Assign(THTMLStatusPanel(Source).Progress);
    Hint := THTMLStatusPanel(Source).Hint;
    Tag := THTMLStatusPanel(Source).Tag;
    Name := THTMLStatusPanel(Source).Name;
  end
  else inherited Assign(Source);
end;


procedure THTMLStatusPanel.SetBiDiMode(Value: TBiDiMode);
begin
  if Value <> FBiDiMode then
  begin
    FBiDiMode := Value;
    FParentBiDiMode := False;
    Changed(False);
  end;
end;

function THTMLStatusPanel.IsBiDiModeStored: Boolean;
begin
  Result := not FParentBiDiMode;
end;

procedure THTMLStatusPanel.SetParentBiDiMode(Value: Boolean);
begin
  if FParentBiDiMode <> Value then
  begin
    FParentBiDiMode := Value;
    ParentBiDiModeChanged;
  end;
end;

procedure THTMLStatusPanel.ParentBiDiModeChanged;
begin
  if FParentBiDiMode then
  begin
    if GetOwner <> nil then
    begin
      BiDiMode := THTMLStatusPanels(GetOwner).FStatusBar.BiDiMode;
      FParentBiDiMode := True;
    end;
  end;
end;


function THTMLStatusPanel.UseRightToLeftReading: Boolean;
begin
  Result := SysLocale.MiddleEast and (BiDiMode <> bdLeftToRight);
end;

function THTMLStatusPanel.UseRightToLeftAlignment: Boolean;
begin
  Result := SysLocale.MiddleEast and (BiDiMode = bdRightToLeft);
end;

function THTMLStatusPanel.GetDisplayName: string;
begin
  Result := Text;
  if Result = '' then Result := inherited GetDisplayName;
end;

procedure THTMLStatusPanel.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed(False);
  end;
end;

procedure THTMLStatusPanel.SetBevel(Value: THTMLStatusPanelBevel);
begin
  if FBevel <> Value then
  begin
    FBevel := Value;
    Changed(False);
  end;
end;

procedure THTMLStatusPanel.SetStyle(Value: THTMLStatusPanelStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed(False);
  end;
end;

procedure THTMLStatusPanel.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed(False);
  end;
end;

procedure THTMLStatusPanel.SetWidth(Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed(True);
  end;
end;

procedure THTMLStatusPanel.SetDateFormat(const Value: string);
begin
  FDateFormat := Value;
  Changed(True);
end;

procedure THTMLStatusPanel.SetTimeFormat(const Value: string);
begin
  FTimeFormat := Value;
  Changed(True);
end;


procedure THTMLStatusPanel.SetProgressStyle(const Value: TProgressStyle);
begin
  FProgressStyle.Assign(Value);
  Changed(True);
end;

procedure THTMLStatusPanel.SetHint(const Value: string);
begin
  fHint := Value;
  Changed(True);
end;

destructor THTMLStatusPanel.Destroy;
begin
  FProgressStyle.Free;
  FImageIndexes.Free;
  inherited;
end;

procedure THTMLStatusPanel.SetHTMLOffsetX(const Value: Integer);
begin
  FHTMLOffsetX := Value;
  Changed(true);
end;

procedure THTMLStatusPanel.SetImageIndex(const Value: Integer);
begin
  FImageIndex := value;
  Changed(true);
end;

procedure THTMLStatusPanel.SetHTMLOffseTY(const Value: Integer);
begin
  FHTMLOffsetY := Value;
  Changed(true);
end;

function THTMLStatusPanel.GetImageIndexes(Index: Integer): Integer;
begin
  Result := -1;
  if FImageIndexes.Count > Index then
    Result := StrToInt(FImageIndexes.Strings[Index]);
end;

procedure THTMLStatusPanel.SetImageIndexes(Index: Integer;
  const Value: Integer);
begin
  while FImageIndexes.Count <= Index do
    FImageIndexes.Add('-1');

  FImageIndexes.Strings[Index] := IntToStr(Value);
end;

procedure THTMLStatusPanel.ClearImageIndexes;
begin
  FImageIndexes.Clear;
end;

function THTMLStatusPanel.ImageCount: Integer;
begin
  Result := FImageIndexes.Count;
end;

procedure THTMLStatusPanel.SetAnimationImages(const Value: TImageList);
begin
  FAnimationImages := Value;
  Changed(true);
end;

procedure THTMLStatusPanel.SetAnimated(const Value: Boolean);
begin
  FAnimated := Value;
  Changed(true);
end;

procedure THTMLStatusPanel.SetAnimationDelay(const Value: Integer);
begin
  if Value >= 0 then
    FAnimationDelay := Value;
    
end;

procedure THTMLStatusPanel.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  Changed(true);
end;

{ THTMLStatusPanels }

constructor THTMLStatusPanels.Create(StatusBar: THTMLStatusBar);
begin
  inherited Create(THTMLStatusPanel);
  FStatusBar := StatusBar;
end;

function THTMLStatusPanels.Add: THTMLStatusPanel;
begin
  Result := THTMLStatusPanel(inherited Add);
end;

function THTMLStatusPanels.GetItem(Index: Integer): THTMLStatusPanel;
begin
  Result := THTMLStatusPanel(inherited GetItem(Index));
end;

function THTMLStatusPanels.GetOwner: TPersistent;
begin
  Result := FStatusBar;
end;

procedure THTMLStatusPanels.SetItem(Index: Integer; Value: THTMLStatusPanel);
begin
  inherited SetItem(Index, Value);
end;

procedure THTMLStatusPanels.Update(Item: TCollectionItem);
begin
  if (Item <> nil) then
    FStatusBar.UpdatePanel(Item.Index, False)
  else
    FStatusBar.UpdatePanels(True, False);
end;

{ THTMLStatusBar }

constructor THTMLStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csDoubleClicks, csOpaque];
  Color := clBtnFace;
  Height := 19;
  Align := alBottom;
  FPanels := THTMLStatusPanels.Create(Self);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FSizeGrip := True;
  ParentFont := False;
  FURLColor := clBlue;
  FUseSystemFont := True;
  SyncToSystemFont;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FMousepanel := -1;
  FTimerCount := 0;
end;

destructor THTMLStatusBar.Destroy;
begin
  FCanvas.Free;
  FPanels.Free;
  FPanels := nil;
  inherited Destroy;
end;

procedure THTMLStatusBar.CreateParams(var Params: TCreateParams);
const
  GripStyles: array[Boolean] of DWORD = (CCS_TOP, SBARS_SIZEGRIP);
begin
  InitCommonControl(ICC_BAR_CLASSES);
  inherited CreateParams(Params);
  CreateSubClass(Params, STATUSCLASSNAME);
  with Params do
  begin
    Style := Style or GripStyles[FSizeGrip and (Parent is TCustomForm)
 and (TCustomForm(Parent).BorderStyle in [bsSizeable, bsSizeToolWin])];
    if not FSizeGrip then
      WindowClass.style := WindowClass.style and not CS_HREDRAW;
  end;
end;

procedure THTMLStatusBar.DestroyWnd;
begin
  KillTimer(handle, fTimerID);
  inherited DestroyWnd;
end;

procedure THTMLStatusBar.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(Handle, SB_SETBKCOLOR, 0, ColorToRGB(Color));
  UpdatePanels(True, False);
  if FSimpleText <> '' then
    SendMessage(Handle, SB_SETTEXT, 255, LParam(PChar(FSimpleText)));
  if FSimplePanel then
    SendMessage(Handle, SB_SIMPLE, 1, 0);

  FTimerID := SetTimer(handle, 111, 100, nil);
end;

function THTMLStatusBar.DoHint: Boolean;
begin
  if Assigned(FOnHint) then
  begin
    FOnHint(Self);
    Result := True;
  end
  else Result := False;
end;

procedure THTMLStatusBar.DrawPanel(Panel: THTMLStatusPanel; const Rect: TRect);
var
  anchor, stripped: string;
  xsize, ysize, i: Integer;
  Settings: TGaugeSettings;
  r, dr, drect: TRect;
  DTSTYLE: DWORD;

  HyperLinks,MouseLink: Integer;
  Focusanchor: string;
  re: TRect;

begin
  r := Rect;

  drect := rect;

  if (Panel.Index = Panels.Count - 1) then
  begin
    Canvas.FillRect(r);
    if SizeGrip then
    begin
      drect.Right := drect.Right - 16;
      r.Right := r.Right - 16;
    end;
  end;

  case Panel.Style of
    psHTML:
      begin
        dr := drect;
        dr.Left := dr.Left + Panel.HTMLOffsetX;
        dr.Top := dr.Top + Panel.HTMLOffsetY;
        // HTMLDraw(Canvas, panel.Text, dr, fImages, 0, 0, false, false, false, false, (fTimerCount > 5), true, 1.0, URLColor, anchor, stripped, xsize, ysize);
        HTMLDrawEx(Canvas, panel.Text, dr, fImages, 0, 0, -1, -1, 1, false, false, false, false, (fTimerCount > 5), false,
        true, 1.0, URLColor, clNone, clNone, clGray, anchor, stripped, focusanchor, xsize, ysize,
        hyperlinks, mouselink, re, nil , FContainer, 0);
      end;
    psEllipsText:
      begin
        DTSTYLE := DT_SINGLELINE or DT_END_ELLIPSIS or DT_VCENTER;
        case Panel.Alignment of
          taLeftJustify: r.Left := r.Left + 2;
          taCenter: DTSTYLE := DTSTYLE or DT_CENTER;
          taRightJustify: DTSTYLE := DTSTYLE or DT_RIGHT;
        end;
        DrawText(Canvas.Handle, PChar(Panel.Text), Length(Panel.Text), r, DTSTYLE);
      end;
    psFileEllipsText:
      begin
        DTSTYLE := DT_SINGLELINE or DT_PATH_ELLIPSIS or DT_VCENTER;
        case Panel.Alignment of
          taLeftJustify: r.Left := r.Left + 2;
          taCenter: DTSTYLE := DTSTYLE or DT_CENTER;
          taRightJustify: DTSTYLE := DTSTYLE or DT_RIGHT;
        end;
        DrawText(Canvas.Handle, PChar(Panel.Text), Length(Panel.Text), r, DTSTYLE);
      end;
    psImage:
      begin
        if Assigned(Images) then
        begin
          r.Left := r.Left + 2;
          dr := r;
          dr.Top := dr.Top + (r.Bottom - r.Top - Images.Height) div 2;
          Images.Draw(Canvas, dr.Left, dr.Top, Panel.ImageIndex);
          r.Left := r.Left + Images.Width;
        end;
        r.Left := r.Left + 2;
        DTSTYLE := DT_SINGLELINE or DT_END_ELLIPSIS or DT_VCENTER;
        case Panel.Alignment of
          taCenter: DTSTYLE := DTSTYLE or DT_CENTER;
          taRightJustify: DTSTYLE := DTSTYLE or DT_RIGHT;
        end;
        DrawText(Canvas.Handle, PChar(Panel.Text), Length(Panel.Text), r, DTSTYLE);
      end;
    psImageList:
      begin
        if Assigned(Images) then
        begin
          for i := 1 to Panel.ImageCount do
          begin
            r.Left := r.Left + 2;
            dr := r;
            dr.Top := dr.Top + (r.Bottom - r.Top - Images.Height) div 2;
            Images.Draw(Canvas, dr.Left, dr.Top, Panel.ImageIndexes[i - 1]);
            r.Left := r.Left + Images.Width;
          end;
        end;
      end;
    psAnimation:
      begin
        if Assigned(Panel.AnimationImages) then
        begin
          r.Left := r.Left + 2;
          dr := r;
          dr.Top := dr.Top + (r.Bottom - r.Top - Panel.AnimationImages.Height) div 2;

          if Panel.Animated then
            Panel.AnimationImages.Draw(Canvas, dr.Left, dr.Top, Panel.FAnimIndex)
          else
            Panel.AnimationImages.Draw(Canvas, dr.Left, dr.Top, 0);
          r.Left := r.Left + Panel.AnimationImages.Width;
        end;
        r.Left := r.Left + 2;
        DTSTYLE := DT_SINGLELINE or DT_END_ELLIPSIS or DT_VCENTER;
        case Panel.Alignment of
          taCenter: DTSTYLE := DTSTYLE or DT_CENTER;
          taRightJustify: DTSTYLE := DTSTYLE or DT_RIGHT;
        end;
        DrawText(Canvas.Handle, PChar(Panel.Text), Length(Panel.Text), r, DTSTYLE);
      end;
    psProgress:
      begin
        Settings.Level0Color := Panel.Progress.Level0Color;
        Settings.Level0ColorTo := Panel.Progress.Level0ColorTo;
        Settings.Level1Color := Panel.Progress.Level1Color;
        Settings.Level1ColorTo := Panel.Progress.Level1ColorTo;
        Settings.Level2Color := Panel.Progress.Level2Color;
        Settings.Level2ColorTo := Panel.Progress.Level2ColorTo;
        Settings.Level3Color := Panel.Progress.Level3Color;
        Settings.Level3ColorTo := Panel.Progress.Level3ColorTo;
        Settings.Level1Perc := Panel.Progress.Level1Perc;
        Settings.Level2Perc := Panel.Progress.Level2Perc;
        Settings.ShowBorder := Panel.Progress.ShowBorder;
        Settings.Stacked := Panel.Progress.Stacked;
        Settings.ShowPercentage := Panel.Progress.ShowPercentage;
        Settings.IsPercent := Panel.Progress.Indication = piPercentage;
        Settings.CompletionSmooth := Panel.Progress.CompletionSmooth;
        Settings.ShowGradient := Panel.Progress.ShowGradient;
        Settings.Font := Canvas.Font;
        Settings.Font.Color := Panel.Progress.TextColor;
        Settings.Orientation := goHorizontal;
        Settings.Steps := 11;
        Settings.BackgroundColor := Panel.Progress.BackGround;
        Settings.BorderColor := Panel.Progress.BorderColor;
        Settings.Position := Panel.Progress.Position;
        Settings.Suffix := Panel.Progress.Suffix;
        Settings.Prefix := Panel.Progress.Prefix;
        if Panel.Progress.Indication = piPercentage then
          DrawGauge(Canvas, r, panel.progress.position, Settings)
        else
        begin
          if Panel.Progress.Max <> Panel.Progress.Min then // avoid division by zero
            DrawGauge(Canvas, r, Round(100 * (Panel.Progress.Position - Panel.Progress.Min) / (Panel.Progress.Max - Panel.Progress.Min)), Settings)
          else
            DrawGauge(Canvas, r, 0, Settings)
        end;
      end;
    psText:
      begin
        FCanvas.FillRect(drect);
        DTSTYLE := DT_SINGLELINE or DT_END_ELLIPSIS or DT_VCENTER;
        case Panel.Alignment of
          taCenter: DTSTYLE := DTSTYLE or DT_CENTER;
          taRightJustify: DTSTYLE := DTSTYLE or DT_RIGHT;
        end;

        if not Panel.Enabled then
          Canvas.Font.Color := clSilver;

        DrawText(Canvas.Handle, PChar(Panel.Text), Length(Panel.Text), r, DTSTYLE);

      end;
    psOwnerDraw:
      begin
        if Assigned(FOnDrawPanel) then
          FOnDrawPanel(Self, Panel, drect)
      end;
  end; {of case}

end;

procedure THTMLStatusBar.SetPanels(Value: THTMLStatusPanels);
begin
  FPanels.Assign(Value);
end;

procedure THTMLStatusBar.SetSimplePanel(Value: Boolean);
begin
  if FSimplePanel <> Value then
  begin
    FSimplePanel := Value;
    if HandleAllocated then
      SendMessage(Handle, SB_SIMPLE, Ord(FSimplePanel), 0);
  end;
end;

procedure THTMLStatusBar.DoRightToLeftAlignment(var Str: string;
  AAlignment: TAlignment; ARTLAlignment: Boolean);
begin
  if ARTLAlignment then ChangeBiDiModeAlignment(AAlignment);

  case AAlignment of
    taCenter: Insert(#9, Str, 1);
    taRightJustify: Insert(#9#9, Str, 1);
  end;
end;

procedure THTMLStatusBar.UpdateSimpleText;
const
  RTLReading: array[Boolean] of Longint = (0, SBT_RTLREADING);
begin
  DoRightToLeftAlignment(FSimpleText, taLeftJustify, UseRightToLeftAlignment);
  if HandleAllocated then
    SendMessage(Handle, SB_SETTEXT, 255 or RTLREADING[UseRightToLeftReading],
      LParam(PChar(FSimpleText)));
end;

procedure THTMLStatusBar.SetSimpleText(const Value: string);
begin
  if FSimpleText <> Value then
  begin
    FSimpleText := Value;
    UpdateSimpleText;
  end;
end;


procedure THTMLStatusBar.CMBiDiModeChanged(var Message: TMessage);
var
  Loop: Integer;
begin
  inherited;
  if HandleAllocated then
    if not SimplePanel then
    begin
      for Loop := 0 to Panels.Count - 1 do
        if Panels[Loop].ParentBiDiMode then
          Panels[Loop].ParentBiDiModeChanged;
      UpdatePanels(True, True);
    end
    else
      UpdateSimpleText;
end;


procedure THTMLStatusBar.FlipChildren(AllLevels: Boolean);
var
  Loop, FirstWidth, LastWidth: Integer;
  APanels: THTMLStatusPanels;
begin
  if HandleAllocated and
    (not SimplePanel) and (Panels.Count > 0) then
  begin
    { Get the true width of the last panel }
    LastWidth := ClientWidth;
    FirstWidth := Panels[0].Width;
    for Loop := 0 to Panels.Count - 2 do Dec(LastWidth, Panels[Loop].Width);
    { Flip 'em }
    APanels := THTMLStatusPanels.Create(Self);
    try
      for Loop := 0 to Panels.Count - 1 do with APanels.Add do
          Assign(Self.Panels[Loop]);
      for Loop := 0 to Panels.Count - 1 do
        Panels[Loop].Assign(APanels[Panels.Count - Loop - 1]);
    finally
      APanels.Free;
    end;
    { Set the width of the last panel }
    if Panels.Count > 1 then
    begin
      Panels[Panels.Count - 1].Width := FirstWidth;
      Panels[0].Width := LastWidth;
    end;
    UpdatePanels(True, True);
  end;
end;

procedure THTMLStatusBar.SetSizeGrip(Value: Boolean);
begin
  if FSizeGrip <> Value then
  begin
    FSizeGrip := Value;
    RecreateWnd;
  end;
end;

procedure THTMLStatusBar.SyncToSystemFont;
begin
  if FUseSystemFont then
    Font := Screen.HintFont;
end;

procedure THTMLStatusBar.UpdatePanel(Index: Integer; Repaint: Boolean);
var
  Flags: Integer;
  S: string;
  PanelRect: TRect;
begin
  if HandleAllocated then
    with Panels[Index] do
    begin
      if not Repaint then
      begin
        FUpdateNeeded := True;
        SendMessage(Handle, SB_GETRECT, Index, LParam(@PanelRect));
        InvalidateRect(Handle, @PanelRect, True);
        Exit;
      end
      else
        if not FUpdateNeeded then Exit;
      FUpdateNeeded := False;
      Flags := 0;
      case Bevel of
        pbNone: Flags := SBT_NOBORDERS;
        pbRaised: Flags := SBT_POPOUT;
      end;
      if UseRightToLeftReading then Flags := Flags or SBT_RTLREADING;

      if (Style in [psHTML, psOwnerDraw, psProgress, psImage, psImageList, psAnimation, psEllipsText, psFileEllipsText ]) or not Enabled then
        Flags := Flags or SBT_OWNERDRAW;

      if (Style = psText) and (Alignment <> taLeftJustify) then
        Flags := Flags or SBT_OWNERDRAW;

      S := Text;
      if UseRightToLeftAlignment then
        DoRightToLeftAlignment(S, Alignment, UseRightToLeftAlignment)
      else
        case Alignment of
          taCenter: Insert(#9, S, 1);
          taRightJustify: Insert(#9#9, S, 1);
        end;
      SendMessage(Handle, SB_SETTEXT, Index or Flags, LParam(PChar(S)));
    end;
end;

procedure THTMLStatusBar.UpdatePanels(UpdateRects, UpdateText: Boolean);
const
  MaxPanelCount = 128;
var
  I, Count, PanelPos: Integer;
  PanelEdges: array[0..MaxPanelCount - 1] of Integer;
begin
  if HandleAllocated then
  begin
    Count := Panels.Count;
    if UpdateRects then
    begin
      if Count > MaxPanelCount then Count := MaxPanelCount;
      if Count = 0 then
      begin
        PanelEdges[0] := -1;
        SendMessage(Handle, SB_SETPARTS, 1, LParam(@PanelEdges));
        SendMessage(Handle, SB_SETTEXT, 0, LParam(PChar('')));
      end else
      begin
        PanelPos := 0;
        for I := 0 to Count - 2 do
        begin
          Inc(PanelPos, Panels[I].Width);
          PanelEdges[I] := PanelPos;
        end;
        PanelEdges[Count - 1] := -1;
        SendMessage(Handle, SB_SETPARTS, Count, LParam(@PanelEdges));
      end;
    end;
    for I := 0 to Count - 1 do
      UpdatePanel(I, UpdateText);
  end;
end;

procedure THTMLStatusBar.CMWinIniChange(var Message: TMessage);
begin
  inherited;
  if (Message.WParam = 0) or (Message.WParam = SPI_SETNONCLIENTMETRICS) then
    SyncToSystemFont;
end;

procedure THTMLStatusBar.CNDrawItem(var Message: TWMDrawItem);
var
  SaveIndex: Integer;
begin
  with Message.DrawItemStruct^ do
    begin
      SaveIndex := SaveDC(hDC);
      FCanvas.Lock;
      try
        FCanvas.Handle := hDC;
        FCanvas.Font.Assign(Font);
        FCanvas.Brush.Color := self.Color;
        FCanvas.Brush.Style := bsSolid;
        DrawPanel(Panels[itemID], rcItem);
      finally
        FCanvas.Handle := 0;
        FCanvas.Unlock;
        RestoreDC(hDC, SaveIndex);
      end;
    end;
  Message.Result := 1;
end;

procedure THTMLStatusBar.WMGetTextLength(var Message: TWMGetTextLength);
begin
  Message.Result := Length(FSimpleText);
end;

procedure THTMLStatusBar.WMPaint(var Message: TWMPaint);
begin
  UpdatePanels(False, True);
  inherited;
end;

procedure THTMLStatusBar.UpdateStatusBar;
var
  i: Integer;
  s: string;
begin
  if (csDestroying in ComponentState) then
    Exit;

  for i := 1 to Panels.Count do
  begin
    case Panels[i - 1].Style of
      psHTML: if (pos('<BLINK', uppercase(Panels[i - 1].Text)) <> 0) and (FTimerCount in [5, 10]) then
        begin
          s := Panels[i - 1].Text;
          Panels[i - 1].Text := '';
          Panels[i - 1].Text := s;
        end;
      psAnimation:
        begin
          if Assigned(Panels[i - 1].AnimationImages) then
          begin
            if FTimerCount mod (Panels[i - 1].AnimationDelay + 1) = 0 then
            begin
              if Panels[i - 1].FAnimIndex < Panels[i - 1].AnimationImages.Count - 1 then
                Panels[i - 1].FAnimIndex := Panels[i - 1].FAnimIndex + 1
              else
                Panels[i - 1].FAnimIndex := 0;

              s := Panels[i - 1].Text;
              Panels[i - 1].Text := '*';
              Panels[i - 1].Text := s;
            end;  
          end;

        end;
      psTime: Panels[i - 1].Text := FormatDateTime(Panels[i - 1].TimeFormat, Now);
      psDate: Panels[i - 1].Text := FormatDateTime(Panels[i - 1].DateFormat, Now);
      psDateTime: Panels[i - 1].Text := FormatDateTime(Panels[i - 1].DateFormat, Now);
      psNumLock: begin
          if getkeystate(vk_numlock) and $1 = $1 then
            Panels[i - 1].Text := NUMLOCK
          else
            Panels[i - 1].Text := ''
        end;
      psCapsLock: begin
          if getkeystate(vk_capital) and $1 = $1 then
            Panels[i - 1].Text := CAPSLOCK
          else
            Panels[i - 1].Text := ''
        end;
      psScrollLock: begin
          if getkeystate(vk_scroll) and $1 = $1 then
            Panels[i - 1].Text := SCROLLLOCK
          else
            Panels[i - 1].Text := ''
        end;
    end;
  end;
end;

procedure THTMLStatusBar.WMTimer(var Msg: TWMTimer);
begin
  UpdateStatusBar;
  inc(fTimerCount);
  if (fTimerCount > 10) then
    fTimerCount := 0;
end;

procedure THTMLStatusBar.WMSize(var Message: TWMSize);
begin
  { Eat WM_SIZE message to prevent control from doing alignment }
  if not (csLoading in ComponentState) then Resize;
  Repaint;
end;

function THTMLStatusBar.IsFontStored: Boolean;
begin
  Result := not FUseSystemFont and not ParentFont and not DesktopFont;
end;

procedure THTMLStatusBar.SetUseSystemFont(const Value: Boolean);
begin
  if FUseSystemFont <> Value then
  begin
    FUseSystemFont := Value;
    if Value then
    begin
      if ParentFont then ParentFont := False;
      SyncToSystemFont;
    end;
  end;
end;

procedure THTMLStatusBar.CMColorChanged(var Message: TMessage);
begin
  inherited;
  RecreateWnd;
end;

procedure THTMLStatusBar.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  if FUseSystemFont and ParentFont then FUseSystemFont := False;
end;


function THTMLStatusBar.ExecuteAction(Action: TBasicAction): Boolean;
begin
  if AutoHint and (Action is THintAction) and not DoHint then
  begin
    if SimplePanel or (Panels.Count = 0) then
      SimpleText := THintAction(Action).Hint else
      Panels[0].Text := THintAction(Action).Hint;
    Result := True;
  end
  else Result := inherited ExecuteAction(Action);
end;

procedure THTMLStatusBar.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  RecreateWnd;
end;

procedure THTMLStatusBar.CMSysFontChanged(var Message: TMessage);
begin
  inherited;
  SyncToSystemFont;
end;

procedure THTMLStatusBar.ChangeScale(M, D: Integer);
begin
  if UseSystemFont then // status bar size based on system font size
    ScalingFlags := [sfTop];
  inherited;
end;

procedure THTMLStatusBar.SetImages(const Value: TImageList);
begin
  FImages := Value;
  Invalidate;
end;

procedure THTMLStatusBar.Notification(AComponent: TComponent;
  AOperation: TOperation);
var
  i: Integer;
begin

  if (AOperation = opRemove) and not (csDestroying in ComponentState) then
  begin
    if (AComponent = FImages) then
      FImages := nil;

    if Assigned(Panels) then
    begin
      for i := 1 to Panels.Count do
      begin
        if AComponent = Panels[i - 1].AnimationImages then
          Panels[i - 1].AnimationImages := nil;
      end;
    end;
  end;

  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;
    
  inherited;
end;

procedure THTMLStatusBar.Resize;
begin
  inherited;
end;

procedure THTMLStatusBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  anchor: string;
  idx: Integer;
begin
  anchor := IsAnchor(x, y);

  idx := GetPanel(x);
  if fMousePanel <> idx then
  begin
    Application.CancelHint;
    FMousePanel := idx;
  end;

  if (Anchor <> '') then
  begin
    if (self.Cursor = crDefault) or (fAnchor <> Anchor) then
    begin
      fAnchor := Anchor;
      self.Cursor := crHandPoint;
      if fAnchorHint then
        Application.CancelHint;
      if Assigned(fAnchorEnter) then fAnchorEnter(self, anchor);
    end;
  end
  else
  begin
    if (self.Cursor = crHandPoint) then
    begin
      self.Cursor := crDefault;
      if assigned(fAnchorExit) then fAnchorExit(self, anchor);
    end;
  end;
  inherited;
end;

function THTMLStatusBar.IsAnchor(x, y: integer): string;
var
  r: trect;
  xsize, ysize: integer;
  anchor, stripped: string;
  idx: integer;

  HyperLinks,MouseLink: Integer;
  Focusanchor: string;
  re: TRect;
begin
  idx := GetPanel(x);
  if (idx < 0) then
    Exit;

  Sendmessage(Handle, SB_GETRECT, idx, LParam(@r));

  Anchor := '';
  //if HTMLDraw(Canvas, Panels.Items[idx].Text, r, FImages, x, y, true, false, false, true, true, true, 1.0, FURLColor, anchor, stripped, xsize, ysize) then
  if HTMLDrawEx(Canvas, Panels.Items[idx].Text, r, FImages, x, y, -1, -1, 1, true, false, false, true, true, false, true,
     1.0, FURLColor, clNone, clNone, clGray, anchor, stripped, focusanchor, xsize, ysize, hyperlinks,
     mouselink, re, nil, FContainer, 0) then
     
    Result := anchor;
end;

function THTMLStatusBar.GetPanel(x: integer): integer;
var
  r: TRect;
  i: Integer;
begin
  Result := -1;
  for i := 1 to panels.Count do
  begin
    Sendmessage(Handle, SB_GETRECT, i - 1, LParam(@r));
    if (x >= r.left) and (x <= r.right) then
    begin
      Result := i - 1;
      break;
    end;
  end;
end;


procedure THTMLStatusBar.CMHintShow(var Msg: TMessage);
var
  CanShow: Boolean;
  hi: PHintInfo;
  anchor: string;
  idx: integer;
begin
  CanShow := True;
  hi := PHintInfo(Msg.LParam);

  idx := GetPanel(hi^.cursorPos.x);

  if (idx >= 0) and (idx < Panels.Count) then
  begin
    if Panels[idx].Style = psHTML then
    begin
      if FAnchorHint then
      begin
        anchor := IsAnchor(hi^.cursorPos.x, hi^.cursorpos.y);
        if (anchor <> '') then
        begin
          hi^.HintPos := clienttoscreen(hi^.CursorPos);
          hi^.hintpos.y := hi^.hintpos.y - 10;
          hi^.hintpos.x := hi^.hintpos.x + 10;
          hi^.HintStr := anchor;
        end
        else
        begin
          if (Panels[idx].Hint <> '') then
            hi^.HintStr := Panels[idx].Hint;
        end;
      end
      else
      begin
        if (Panels[idx].Hint <> '') then
          hi^.HintStr := Panels[idx].Hint;
      end;
    end
    else
    begin
      if (Panels[idx].Hint <> '') then hi^.HintStr := Panels[idx].Hint;
    end;
  end;
  Msg.Result := Ord(not CanShow);
end;

procedure THTMLStatusBar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Anchor: string;
  idx: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  idx := GetPanel(x);

  if Assigned(OnPanelCLick) and (Button = mbLeft) then
    OnPanelClick(Self, Idx);

  if Assigned(OnPanelRightCLick) and (Button = mbRight) then
    OnPanelRightClick(Self, Idx);

  Anchor := IsAnchor(X, Y);
  if Anchor <> '' then
  begin
    if (Pos('://', anchor) > 0) or (pos('mailto:', anchor) > 0) then
      ShellExecute(0, 'open', pchar(anchor), nil, nil, SW_NORMAL)
    else
    begin
      if Assigned(fAnchorClick) then
        FAnchorClick(self, anchor);
    end;
  end;
end;

procedure THTMLStatusBar.Loaded;
begin
  inherited;
  UpdateStatusBar;
end;

function THTMLStatusBar.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function THTMLStatusBar.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure THTMLStatusBar.SetVersion(const Value: string);
begin

end;

procedure THTMLStatusBar.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  idx: Integer;
begin
  inherited;
  if Assigned(OnPanelDblClick) then
  begin
    idx := GetPanel(Message.XPos);
    OnPanelDblClick(Self, idx);
  end;
end;

{ TProgressStyle }

procedure TProgressStyle.Assign(Source: TPersistent);
begin
  //FColor := (Source as TProgressStyle).Color;
  FBackGround := (Source as TProgressStyle).Background;
  FTextColor := (Source as TProgressStyle).TextColor;
  FIndication := (Source as TProgressStyle).Indication;
  FMin := (Source as TProgressStyle).Min;
  FMax := (Source as TProgressStyle).Max;
  FPosition := (Source as TProgressStyle).Position;
end;

procedure TProgressStyle.Changed;
var
  r: trect;
begin
  // optimized repaint
  SendMessage(THTMLStatusPanels(FOwner.Collection).FStatusbar.handle, SB_GETRECT, FOwner.Index, LParam(@r));
  InvalidateRect(THTMLStatusPanels(FOwner.Collection).FStatusbar.handle, @r, true);

  // fOwner.Changed(true);
end;

constructor TProgressStyle.Create(aOwner: THTMLStatusPanel);
begin
  inherited Create;
  fBackground := clBtnFace;
  fTextColor := clBlack;
  fMin := 0;
  fMax := 100;
  fOwner := aOwner;

  FLevel0Color := clLime;
  FLevel0ColorTo := $00E1FFE1;
  FLevel1Color := clYellow;
  FLevel1ColorTo := $00CAFFFF;
  FLevel2Color := $0053A9FF;
  FLevel2ColorTo := $00A8D3FF;
  FLevel3Color := clRed;
  FLevel3ColorTo := $00CACAFF;

  FLevel1Perc := 70;
  FLevel2Perc := 90;
  FShowGradient := True;
  FCompletionSmooth := False;
  FShowPercentage := True;
end;

destructor TProgressStyle.Destroy;
begin
  inherited;
end;

procedure TProgressStyle.SetBackGround(const Value: tColor);
begin
  FBackground := Value;
  Changed;
end;

procedure TProgressStyle.SetIndication(const Value: TProgressIndication);
begin
  FIndication := Value;
  Changed;
end;

procedure TProgressStyle.SetMax(const Value: integer);
begin
  FMax := Value;
  Changed;
end;

procedure TProgressStyle.SetMin(const Value: integer);
begin
  FMin := Value;
  Changed;
end;

procedure TProgressStyle.SetPosition(const Value: integer);
begin
  if (FPosition <> Value) then
  begin
    FPosition := Value;
    Changed;
  end;
end;

procedure TProgressStyle.SetTextColor(const Value: tColor);
begin
  FTextColor := Value;
  Changed;
end;

procedure TProgressStyle.SetLevel0Color(const Value: tColor);
begin
  fLevel0Color := Value;
  Changed;
end;

procedure TProgressStyle.SetLevel0ColorTo(const Value: tColor);
begin
  fLevel0ColorTo := Value;
  Changed;
end;

procedure TProgressStyle.SetLevel1Color(const Value: tColor);
begin
  fLevel1Color := Value;
  Changed;
end;

procedure TProgressStyle.SetLevel1ColorTo(const Value: tColor);
begin
  fLevel1ColorTo := Value;
  Changed;
end;

procedure TProgressStyle.SetLevel2Color(const Value: tColor);
begin
  fLevel2Color := Value;
  Changed;
end;

procedure TProgressStyle.SetLevel2ColorTo(const Value: tColor);
begin
  fLevel2ColorTo := Value;
  Changed;
end;

procedure TProgressStyle.SetLevel3Color(const Value: tColor);
begin
  fLevel3Color := Value;
  Changed;
end;

procedure TProgressStyle.SetLevel3ColorTo(const Value: tColor);
begin
  fLevel3ColorTo := Value;
  Changed;
end;

procedure TProgressStyle.SetLevel1Perc(Value: integer);
begin
  FLevel1Perc := Value;
  Changed;
end;

procedure TProgressStyle.SetLevel2Perc(Value: integer);
begin
  FLevel2Perc := Value;
  Changed;
end;

procedure TProgressStyle.SetBorderColor(const Value: tColor);
begin
  FBorderColor := Value;
  Changed;
end;

procedure TProgressStyle.SetShowBorder(Value: boolean);
begin
  FShowBorder := Value;
  Changed;
end;

procedure TProgressStyle.SetStacked(Value: boolean);
begin
  FStacked := Value;
  Changed;
end;

procedure TProgressStyle.SetShowPercentage(Value: boolean);
begin
  FShowPercentage := Value;
  Changed;
end;

procedure TProgressStyle.SetCompletionSmooth(Value: boolean);
begin
  FCompletionSmooth := Value;
  Changed;
end;

procedure TProgressStyle.SetShowGradient(Value: boolean);
begin
  FShowGradient := Value;
  Changed;
end;

procedure TProgressStyle.SetPrefix(const Value: string);
begin
  FPrefix := Value;
  Changed;
end;

procedure TProgressStyle.SetSuffix(const Value: string);
begin
  FSuffix := Value;
  Changed;
end;


{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}


end.
