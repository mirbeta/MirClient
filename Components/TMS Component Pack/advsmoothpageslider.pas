{***************************************************************************}
{ TAdvSmoothPageSlider component                                            }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2013 - 2015                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvSmoothPageSlider;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Math, Menus,
  Dialogs, Forms, ExtCtrls, GDIPFill, AdvStyleIF,
  AdvGDIP, axCtrls, ImgList, GDIPPictureContainer, Types
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
  // v1.0.0.0 : First Release
  // v1.0.0.1 : Fixed : Issue with page slider when minimumwidth = 0
  // v1.0.0.2 : Fixed : Issue with OnPageChange and OnChange events pointing to the same event declaration
  //          : Fixed : Issue with repainting with some controls
  // v1.0.0.3 : Fixed : Issue with repainting when animating
  // v1.0.0.4 : Fixed : Issue with shadow not showing
  // v1.0.0.5 : Fixed : Issue with 64bit and Windows 8
  // v1.1.0.0 : New : Windows 8, Office 2013 styles added
  // v1.1.0.1 : New : AllowPageDrag property
  // v1.1.0.2 : Improved : Hide shadow when changing visibility of pages
  // v1.1.0.3 : Fixed : Issue with shadow when activating/deactivating form and changing visibility of pages
  // v1.1.0.4 : Fixed : Issue with formhooking on empty page slider
  // v1.2.0.0 : New : Windows 10, Office 2016 styles added
  // v1.2.0.1 : Fixed : Issue with setting active page index programmatically
  
  WM_USERACTIVATE = WM_USER + 100;
  SHADOWSIZE = 12;

type

  {$IFNDEF DELPHI2006_LVL}
  TVerticalAlignment = (taAlignTop, taAlignBottom, taVerticalCenter);
  {$ENDIF}
  {$IFNDEF DELPHI7_LVL}
  TWMMoving = record
    Msg: Cardinal;
    fwSide: Cardinal;
    DragRect: PRect;
    Result: Integer;
  end;
  {$ENDIF}

  TAdvSmoothPageSlider = class;
  TAdvSmoothPage = class;

  TDbgList = class(TList)
  private
    function GetItemsEx(Index: Integer): Pointer;
    procedure SetItemsEx(Index: Integer; const Value: Pointer);
  public
    property Items[Index: Integer]: Pointer read GetItemsEx write SetItemsEx; default;
  end;

  TProWinControl = class(TWinControl);

  TAdvSmoothPageShadow = class(TForm)
  private
    FMouseEntered: Boolean;
    OldWndProc, NewWndProc: Pointer;
    FMainBuffer: TGPBitmap;
    FPage: TAdvSmoothPage;
    FW: Integer;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMMouseActivate(var Msg: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMActivate(var Message: TMessage); message WM_ACTIVATE;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure SetW(const Value: Integer);
  protected
    procedure FormHookInit;
    procedure FormHookDone;
    procedure CreateWnd; override;
    procedure DoCreate; override;
    procedure DoDestroy; override;
    procedure Paint; override;
    procedure Draw(graphics: TGPGraphics);
    procedure CreateMainBuffer;
    procedure DestroyMainBuffer;
    procedure ClearBuffer(graphics: TGPGraphics);
    function CreateGraphics: TGPGraphics;
    procedure SetLayeredWindow;
    procedure UpdateLayered;
    procedure UpdateMainWindow;
    procedure UpdateWindow;
    procedure WndProc(var Message: TMessage); override;
    procedure HookWndProc(var Msg: TMessage);
    property w: Integer read FW write SetW;
  public
    procedure Init;
    property OwnerPage: TAdvSmoothPage read FPage write FPage;
  end;

  TAdvSmoothPage = class(TCustomControl, ITMSStyle)
  private
    FTMSStyle: TTMSStyle;
    FDownX, FDownY: Integer;
    FShadowForm: TAdvSmoothPageShadow;
    FLeftTemp: Integer;
    FBlockUpdate: Boolean;
    FAnimating: Boolean;
    FCurrentPos: Integer;
    FMetroStyle: Boolean;
    FDesignTime: Boolean;
    FPageIndex: integer;
    FPageSlider: TAdvSmoothPageSlider;
    FUpdatingParent: Boolean;
    FHeaderFill: TGDIPFill;
    FShowHeader: Boolean;
    FHeaderHeight: Integer;
    FHeader: String;
    FFooterFill: TGDIPFill;
    FShowFooter: Boolean;
    FFooterHeight: Integer;
    FFooter: String;
    FFill: TGDIPFill;
    FFooterFont: TFont;
    FHeaderFont: TFont;
    FMinimumWidth: Integer;
    FUpdateFormBounds: Boolean;
    FPictureContainer: TGDIPPictureContainer;
    FImageList: TCustomImageList;
    FShadowColor: TColor;
    FHeaderVerticalAlignment: TVerticalAlignment;
    FHeaderHorizontalAlignment: TAlignment;
    FFooterHorizontalAlignment: TAlignment;
    FFooterVerticalAlignment: TVerticalAlignment;
    procedure WMMoving(var Message: TWMMoving); message WM_MOVING;
    procedure WMEnterSizeMove(var Message: TMessage) ; message WM_ENTERSIZEMOVE;
    procedure WMExitSizeMove(var Message: TMessage) ; message WM_EXITSIZEMOVE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure SetPageSlider(const Value: TAdvSmoothPageSlider);
    function GetPageIndex: Integer;
    procedure SetPageIndex(const Value: Integer);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetFooter(const Value: String);
    procedure SetFooterFill(const Value: TGDIPFill);
    procedure SetFooterHeight(const Value: Integer);
    procedure SetHeader(const Value: String);
    procedure SetHeaderFill(const Value: TGDIPFill);
    procedure SetHeaderHeight(const Value: Integer);
    procedure SetShowFooter(const Value: Boolean);
    procedure SetShowHeader(const Value: Boolean);
    procedure SetFooterFont(const Value: TFont);
    procedure SetHeaderFont(const Value: TFont);
    procedure SetMinimumWidth(const Value: Integer);
    procedure SetCurrentPos(const Value: Integer);
    function GetCurrentPos: Integer;
    procedure SetImageList(const Value: TCustomImageList);
    procedure SetPictureContainer(const Value: TGDIPPictureContainer);
    procedure SetHeaderHorizontalAlignment(const Value: TAlignment);
    procedure SetHeaderVerticalAlignment(const Value: TVerticalAlignment);
    procedure SetFooterHorizontalAlignment(const Value: TAlignment);
    procedure SetFooterVerticalAlignment(const Value: TVerticalAlignment);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateFormSize;
    procedure FormHookInit;
    procedure FormHookDone;  
    procedure CreateWnd; override;
    property CurrentPos: Integer read GetCurrentPos write SetCurrentPos;
    function GetPosTo: Integer;
    procedure ObjectChanged(Sender: TObject);
    function GetHeaderRect: TGPRectF;
    function GetFooterRect: TGPRectF;
    function GetBackGroundRect: TGPRectF;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure Paint; override;
    procedure ReadState(Reader: TReader); override;
    procedure DrawBackGround(g: TGPGraphics);
    procedure DrawHeader(g: TGPGraphics);
    procedure DrawFooter(g: TGPGraphics);
    procedure DrawElement(g: TGPGraphics; ARect: TGPRectF;
      AFill: TGDIPFill; AFont: TFont; AText: String; AShow: Boolean; HAlign: TAlignment; VAlign: TVerticalAlignment);
    procedure ShowForm;
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property PageSlider: TAdvSmoothPageSlider read FPageSlider write SetPageSlider;
    procedure Assign(Source: TPersistent); override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
  published
    property ShadowColor: TColor read FShadowColor write FShadowColor default clBlack;
    property Fill: TGDIPFill read FFill write SetFill;
    property Header: String read FHeader write SetHeader;
    property Footer: String read FFooter write SetFooter;
    property HeaderFill: TGDIPFill read FHeaderFill write SetHeaderFill;
    property FooterFill: TGDIPFill read FFooterFill write SetFooterFill;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property FooterFont: TFont read FFooterFont write SetFooterFont;
    property ShowHeader: Boolean read FShowHeader write SetShowHeader default True;
    property ShowFooter: Boolean read FShowFooter write SetShowFooter default True;
    property HeaderHeight: Integer read FHeaderHeight write SetHeaderHeight default 44;
    property HeaderHorizontalAlignment: TAlignment read FHeaderHorizontalAlignment write SetHeaderHorizontalAlignment default taCenter;
    property HeaderVerticalAlignment: TVerticalAlignment read FHeaderVerticalAlignment write SetHeaderVerticalAlignment default taVerticalCenter;
    property FooterHorizontalAlignment: TAlignment read FFooterHorizontalAlignment write SetFooterHorizontalAlignment default taCenter;
    property FooterVerticalAlignment: TVerticalAlignment read FFooterVerticalAlignment write SetFooterVerticalAlignment default taVerticalCenter;
    property FooterHeight: Integer read FFooterHeight write SetFooterHeight default 44;
    property MinimumWidth: Integer read FMinimumWidth write SetMinimumWidth default 200;
    property ShowHint;
    property PageIndex: Integer read GetPageIndex write SetPageIndex stored false;
    property PopupMenu;
    property OnContextPopup;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDrag;
    property OnStartDrag;
    property OnExit;
    property OnEnter;

    property ImageList: TCustomImageList read FImageList write SetImageList;
    property PictureContainer: TGDIPPictureContainer read FPictureContainer write SetPictureContainer;
  end;

  TPageMovedEvent = procedure(Sender: TObject; FromPos: integer; ToPos: Integer) of object;
  TPageChangingEvent = procedure(Sender: TObject; FromPage, ToPage: Integer; var AllowChange: Boolean) of object;

  TPageChangedEvent = procedure(Sender: TObject; Page: Integer) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothPageSlider = class(TAdvSmoothPage, ITMSStyle, ITMSTones)
  private
    FAnimate: Boolean;
    FTimer: TTimer;
    Ffocused: Boolean;
    FPages: TDbgList;
    FPropertiesLoaded: Boolean;
    FActivePageIndex: Integer;
    FOnChange: TNotifyEvent;
    FFormWndProc: TWndMethod;
    FConstructed, FDesignTime: boolean;
    FShowFocus: Boolean;
    FOnPageMoved: TPageMovedEvent;
    FOnChanging: TPageChangingEvent;
    FAnimationFactor: Integer;
    FOnPageChange: TNotifyEvent;
    FOnPageChanged: TPageChangedEvent;
    FAllowPageDrag: Boolean;
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure SetAllPagesPosition;
    function GetPageCount: integer;
    function GetPage(index: integer): TAdvSmoothPage;
    function GetActivePage: TAdvSmoothPage;
    function GetActivePageIndex: Integer;
    procedure SetActivePage(const Value: TAdvSmoothPage);
    procedure SetActivePageIndex(const Value: Integer);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetShowFocus(const Value: Boolean);
    function GetPageRect: TRect;
    procedure SetAnimationFactor(const Value: Integer);
  protected
    procedure DoTimer(Sender: TObject);
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure WndProc(var Msg: TMessage); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure Paint; override;
    procedure Resize; override;

    procedure ChangeActivePage(PageIndex: Integer);

    procedure DoExit; override;
    procedure DoEnter; override;


    function IsActivePageNeighbour(PageIndex: Integer): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Init;
    function GetVersionNr: integer;
    function AddPage(APage: TAdvSmoothPage): integer; overload;
    function AddPage(PageCaption: TCaption): integer; overload;
    procedure RemovePage(APage: TAdvSmoothPage);
    procedure MovePage(CurIndex, NewIndex: Integer);
    function FindNextPage(CurPage: TAdvSmoothPage; GoForward: Boolean): TAdvSmoothPage;
    procedure SelectNextPage(GoForward: Boolean);
    function IndexOfPage(APage: TAdvSmoothPage): Integer;
    property PageCount: integer read GetPageCount;
    property Pages[index: integer]: TAdvSmoothPage read GetPage;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetColorTones(ATones: TColorTones);
    procedure NextPage;
    procedure PreviousPage;
  published
    property AllowPageDrag: Boolean read FAllowPageDrag write FAllowPageDrag default True;
    property AnimationFactor: Integer read FAnimationFactor write SetAnimationFactor default 5;
    property Align;
    property Anchors;
    property ActivePage: TAdvSmoothPage read GetActivePage write SetActivePage;
    property Constraints;
    property Color;
    property ShowHint;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default true;
    property Version: string read GetVersion write SetVersion stored false;
    property ActivePageIndex: Integer read GetActivePageIndex write SetActivePageIndex;
    property Visible;
    property OnPageMoved: TPageMovedEvent read FOnPageMoved write FOnPageMoved;
    property OnPageChange: TNotifyEvent read FOnPageChange write FOnPageChange;
    property OnPageChanging: TPageChangingEvent read FOnChanging write FOnChanging;
    property OnPageChanged: TPageChangedEvent read FOnPageChanged write FOnPageChanged;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property TabOrder;
    property TabStop default true;
    {$IFDEF DELPHI_TOUCH}
    property OnGesture;
    property Touch;
    {$ENDIF}
  end;

implementation

uses
  CommCtrl, ShellApi;

type
  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}


{$I GDIPHTMLEngine.pas}

function AnimateDouble(var Start, Stop: integer; Delta: Double; Margin: integer): Boolean;
begin
  Result := true;
  if (Start > Stop - Margin) and (Start < Stop + Margin) then
  begin
    Start := Stop;
    Result := false;
  end
  else
  begin
    Delta := Max(1, Delta);
    if Start < Stop then
      Start := Round(Start + Delta)
    else
      Start := Round(Start - Delta);
  end;
end;

procedure DrawFocus(g: TGPGraphics; r: TGPRectF; rn: Integer; rt: TFillRoundingType);
var
  pathfocus: TGPGraphicsPath;
  pfocus: TGPPen;
begin
  pathfocus := GDIPFill.CreateRoundRectangle(r, rn, rt, false);
  g.SetSmoothingMode(SmoothingModeDefault);
  pfocus := TGPPen.Create(MakeColor(255, clBlack), 1);
  pfocus.SetDashStyle(DashStyleDot);
  g.DrawPath(pfocus, pathfocus);
  pfocus.Free;
  pathfocus.Free;
end;

{ TDbgList }

function TDbgList.GetItemsEx(Index: Integer): Pointer;
begin
  if (Index >= Count) then
  begin
    raise Exception.Create('Index out of bounds in list read access');
  end;

  if Index < Count then
    Result := inherited Items[Index]
  else
    Result := nil;
end;

procedure TDbgList.SetItemsEx(Index: Integer; const Value: Pointer);
begin
  if (Index >= Count) then
  begin
    raise Exception.Create('Index out of bounds in list write access');
  end;
  if Index < Count then
    inherited Items[Index] := value;
end;

{ TAdvSmoothPage }

constructor TAdvSmoothPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls] - [csOpaque];

  FDesignTime := (csDesigning in ComponentState) and not
      ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  DoubleBuffered := true;

  FFill := TGDIPFill.Create;
  FFill.OnChange := ObjectChanged;
  FHeaderFill := TGDIPFill.Create;
  FHeaderFill.OnChange := ObjectChanged;
  FFooterFill := TGDIPFill.Create;
  FFooterFill.OnChange := ObjectChanged;

  FFooterFont := TFont.Create;
  FFooterFont.OnChange := ObjectChanged;

  FHeaderFont := TFont.Create;
  FHeaderFont.OnChange := ObjectChanged;

  FHeaderHeight := 44;
  FMinimumWidth := 200;
  FFooterHeight := 44;
  FShowHeader := True;
  FShowFooter := True;

  FFooterHorizontalAlignment := taCenter;
  FFooterVerticalAlignment := taVerticalCenter;
  FHeaderHorizontalAlignment := taCenter;
  FHeaderVerticalAlignment := taVerticalCenter;

  FShadowColor := clBlack;

  if FDesignTime then
  begin

    FHeader := 'Main Page';
    FFooter := 'Footer';
    FFill.BeginUpdate;
    FFill.GradientType := gtSolid;
    FFill.Color := clWhite;
    FFill.BorderColor := RGB(114,120,137);
    FFill.EndUpdate;

    FHeaderFill.BeginUpdate;
    FHeaderFill.GradientType := gtVertical;
    FHeaderFill.ColorTo := RGB(203, 206, 214);
    FHeaderFill.Color := RGB(241,242,245);
    FHeaderFill.BorderColor := RGB(114,120,137);
    FHeaderFill.EndUpdate;

    FFooterFill.BeginUpdate;
    FFooterFill.GradientType := gtVertical;
    FFooterFill.ColorTo := RGB(203, 206, 214);
    FFooterFill.Color := RGB(241,242,245);
    FFooterFill.BorderColor := RGB(114,120,137);
    FFooterFill.EndUpdate;
  end;
end;

procedure TAdvSmoothPage.CreateWnd;
begin
  inherited;
  if not (csDesigning in ComponentState) then  
    ShowForm;
end;

destructor TAdvSmoothPage.Destroy;
begin
  if (FPageSlider <> nil) then
    FPageSlider.RemovePage(Self);

  if Assigned(FShadowForm) then  
    FShadowForm.Free;
  FHeaderFont.Free;
  FFooterFont.Free;
  FFill.Free;
  FHeaderFill.Free;
  FFooterFill.Free;

  inherited;
end;

procedure TAdvSmoothPage.DrawBackGround(g: TGPGraphics);
begin
  Fill.Fill(g, GetBackGroundRect);
end;

procedure TAdvSmoothPage.DrawElement(g: TGPGraphics; ARect: TGPRectF;
  AFill: TGDIPFill; AFont: TFont; AText: String; AShow: Boolean; HAlign: TAlignment; VAlign: TVerticalAlignment);
var
  str: string;
  UseHTML: Boolean;
  rf: TGPRectF;
  r: TRect;
  a, s, f: String;
  xs, ys: Integer;
  hl, ml: Integer;
  hr: TRect;
  ft: TGPFont;
  sf: TGPStringFormat;
  b: TGPSolidBrush;
begin
  if AShow then
  begin
    AFill.Fill(g, ARect);
    rf := MakeRect(ARect.X + 3, ARect.Y + 3, ARect.Width - 6, ARect.Height - 6);
    r := Bounds(Round(rf.X), Round(rf.Y), Round(rf.Width), Round(rf.Height));
    str := AText;
    if str <> '' then
    begin
      UseHTML := (Pos('</', str) > 0) or (Pos('/>', str) > 0) or (Pos('<BR>',uppercase(str)) > 0);
      if UseHTML then
      begin
        HTMLDrawGDIP(g, AFont, str, r, FImageList, -1, -1, -1, -1, -1, False, True, False, False, False, False, False, 0, clBlue,
        clNone, clNone, clNone, a, s, f, xs, ys, hl, ml, hr, nil, FPictureContainer, 0);

        r := Bounds(r.Left, r.Top, xs, ys);

        case HAlign of
          taRightJustify: r := Bounds(Round(rf.Width - xs), r.Top, r.Right - r.Left, r.Bottom - r.Top);
          taCenter: r := Bounds(Round(rf.X + (rf.Width - xs) / 2), r.Top, r.Right - r.Left, r.Bottom - r.Top);
        end;

        case VAlign of
          taAlignBottom: r := Bounds(r.Left, Round(rf.Height + rf.Y - ys), r.Right - r.Left, r.Bottom - r.Top);
          taVerticalCenter: r := Bounds(r.Left, Round(rf.Y + (rf.Height - ys) / 2), r.Right - r.Left, r.Bottom - r.Top);
        end;

        HTMLDrawGDIP(g, AFont, str, r, FImageList, -1, -1, -1, -1, -1, False, false, False, False, False, False, False, 0, clBlue,
        clNone, clNone, clNone, a, s, f, xs, ys, hl, ml, hr, nil, FPictureContainer, 0);
      end
      else
      begin
        ft := g.MakeFont(AFont);
        sf := TGPStringFormat.Create;
        case HAlign of
          taLeftJustify: sf.SetAlignment(StringAlignmentNear);
          taRightJustify: sf.SetAlignment(StringAlignmentFar);
          taCenter: sf.SetAlignment(StringAlignmentCenter);
        end;
        case VAlign of
          taAlignTop: sf.SetLineAlignment(StringAlignmentNear);
          taAlignBottom: sf.SetLineAlignment(StringAlignmentFar);
          taVerticalCenter: sf.SetLineAlignment(StringAlignmentCenter);
        end;
        b := TGPSolidBrush.Create(MakeColor(255, AFont.Color));
        g.DrawString(str, Length(str), ft, rf, sf, b);
        b.Free;
        sf.Free;
        ft.Free;
      end;
    end;
  end;
end;

procedure TAdvSmoothPage.DrawFooter(g: TGPGraphics);
begin
  DrawElement(g, GetFooterRect, FooterFill, FooterFont, Footer, ShowFooter,
    FooterHorizontalAlignment, FooterVerticalAlignment);
end;

procedure TAdvSmoothPage.DrawHeader(g: TGPGraphics);
begin
  DrawElement(g, GetHeaderRect, HeaderFill, HeaderFont, Header, ShowHeader,
    HeaderHorizontalAlignment, HeaderVerticalAlignment);
end;

procedure TAdvSmoothPage.FormHookDone;
begin
  if Assigned(FShadowForm) then
    FShadowForm.FormHookDone;
end;

procedure TAdvSmoothPage.FormHookInit;
begin
  if Assigned(FShadowForm) then
    FShadowForm.FormHookInit;
end;

procedure TAdvSmoothPage.ObjectChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvSmoothPage.AlignControls(AControl: TControl; var ARect: TRect);
begin
  if Assigned(PageSlider) then
  begin
    if not (AControl is TAdvSmoothPage) then
    begin
      ARect.Bottom := ARect.Bottom - Round(GetFooterRect.Height) - HeaderFill.BorderWidth;
      ARect.Top := ARect.Top + Round(GetHeaderRect.Height) + HeaderFill.BorderWidth;
    end;
  end;
  inherited AlignControls(Acontrol, ARect);
end;

procedure TAdvSmoothPage.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothPage then
  begin
    FPageIndex := (Source as TAdvSmoothPage).PageIndex;
    Changed;
  end;
end;

procedure TAdvSmoothPage.Paint;
var
  g: TGPGraphics;
begin
  inherited;
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  DrawBackGround(g);
  DrawHeader(g);
  DrawFooter(g);
  g.Free;
end;

procedure TAdvSmoothPage.SetPageSlider(const Value: TAdvSmoothPageSlider);
begin
  if (FPageSlider <> Value) then
  begin
    if FPageSlider <> nil then FPageSlider.RemovePage(Self);
    Parent := Value;
    if (Value <> nil) then
    begin
      Value.AddPage(Self);
      if not (csLoading in ComponentState) then
      begin
        if Value.PageCount > 1 then
        begin
          // assign page appearance
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothPage.SetParent(AParent: TWinControl);
var
  ci, ni: Integer;
  APageSlider: TAdvSmoothPageSlider;
begin
  if ((AParent is TAdvSmoothPage) or (AParent is TAdvSmoothPageSlider)) and not (FUpdatingParent) then
  begin
    APageSlider := nil;
    if (AParent is TAdvSmoothPage) then
    begin
      APageSlider := TAdvSmoothPage(AParent).FPageSlider;
    end
    else if (AParent is TAdvSmoothPageSlider) then
    begin
      APageSlider := TAdvSmoothPageSlider(AParent);
    end;

    if Assigned(FPageSlider) and Assigned(APageSlider) then
    begin
      if (FPageSlider <> APageSlider) then
      begin
        FUpdatingParent := True;
        FPageSlider := APageSlider;
        FUpdatingParent := False;
      end;

      if (FPageSlider = APageSlider) then
      begin
        if (AParent is TAdvSmoothPage) then
        begin
          ci := FPageSlider.IndexOfPage(self);
          ni := FPageSlider.IndexOfPage(TAdvSmoothPage(AParent));
          AParent := APageSlider;
          if (ci >= 0) and (ci < FPageSlider.FPages.Count) and
             (ni >= 0) and (ni < FPageSlider.FPages.Count) then
          begin
            FPageSlider.MovePage(ci, ni);
          end
          else
            raise Exception.Create('Invalid Parent '+inttostr(ci)+':'+inttostr(ni));
        end
        else if (AParent is TAdvSmoothPageSlider) then
        begin
          AParent := APageSlider;
        end;

        FPageSlider.Invalidate;
        Invalidate;
      end
      else
        raise Exception.Create('Invalid Parent');
    end;
  end;
  inherited;
end;

procedure TAdvSmoothPage.SetPictureContainer(
  const Value: TGDIPPictureContainer);
begin
  FPictureContainer := Value;
  Invalidate;
end;

procedure TAdvSmoothPage.SetShowFooter(const Value: Boolean);
begin
  if FShowFooter <> Value then
  begin
    FShowFooter := Value;
    Invalidate;
  end;
end;

procedure TAdvSmoothPage.SetShowHeader(const Value: Boolean);
begin
  if FShowHeader <> Value then
  begin
    FShowHeader := Value;
    Invalidate;
  end;
end;

procedure TAdvSmoothPage.ShowForm;
begin
  if {Assigned(FPageSlider) and }(ShadowColor <> clNone) then
  begin
    FShadowForm := TAdvSmoothPageShadow.CreateNew(Self);
    FShadowForm.OwnerPage := Self;
    FShadowForm.Height := Self.Height;
    FShadowForm.Width := SHADOWSIZE;
    FShadowForm.Init;
    FShadowForm.Left := Self.Left + 2 - Width;
    FShadowForm.Top := Self.Top;
    SetWindowPos(FShadowForm.Handle, 0, FShadowForm.Left, FShadowForm.Top,
      FShadowForm.Width, FShadowForm.Height, SWP_SHOWWINDOW or SWP_NOACTIVATE);
  end;
end;

procedure TAdvSmoothPage.UpdateFormSize;
begin
  if not Assigned(FShadowForm) or (csDestroying in ComponentState) then
    Exit;
    
  if not FUpdateFormBounds then
  begin
    FShadowForm.Left := Parent.ClientOrigin.X + Self.Left + 2;
    FShadowForm.Top := Parent.ClientOrigin.Y + Self.Top;
  end
  else
  begin
    if Assigned(FShadowForm) and Assigned(FPageSlider) then
    begin
      FUpdateFormBounds := False;      
      FShadowForm.SetBounds(Parent.ClientOrigin.X + Self.Left + 2 - ShadowSize, Parent.ClientOrigin.Y + Self.Top, SHADOWSIZE, Self.Height);

      if (Left < SHADOWSIZE) and (Left > 0) then 
        FShadowForm.w := Min(SHADOWSIZE, Left)
      else if (Left >= FPageSlider.Width) or (Left <= 0) then
        FShadowForm.w := 0
      else
        FShadowForm.w := SHADOWSIZE;    
    end;
  end;
end;

procedure TAdvSmoothPage.WMEnterSizeMove(var Message: TMessage);
begin
  inherited;
  if not (csDesigning in ComponentState)  and Assigned(PageSlider) then
  begin
    if Assigned(FPageSlider) then
      FPageSlider.FAnimate := False;
  end;
end;

procedure TAdvSmoothPage.WMExitSizeMove(var Message: TMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) and Assigned(PageSlider) then
  begin
    if Assigned(FPageSlider) then
    begin
      if Left < Max(150, MinimumWidth / 2) then
        FPageSlider.NextPage
      else if Left > FPageSlider.Width - Max(150, MinimumWidth) then
        FPageSlider.PreviousPage
    end;

    if Assigned(FPageSlider) then
      FPageSlider.FAnimate := True;
  end;
end;

procedure TAdvSmoothPage.WMMoving(var Message: TWMMoving);
var
  p: TPoint;
  I: Integer;
  page, nextpage: TAdvSmoothPage;
begin
  inherited;
  if not (csDesigning in ComponentState) and Assigned(PageSlider) then
  begin
    if FPageSlider.ActivePageIndex = PageIndex then
    begin
      p := ClientToScreen(Point(Left, Top));
      Message.DragRect.Top := p.Y;
      Message.DragRect.Bottom := p.Y + Self.Height;
      CurrentPos := Left;
      for I :=  0 to PageIndex - 1 do
      begin
        page := TAdvSmoothPage(FPageSlider.FPages[I]);
        nextpage := TAdvSmoothPage(FPageSlider.FPages[I + 1]);
        if Assigned(page) and Assigned(nextpage) then
        begin
          page.CurrentPos := nextpage.Left - page.MinimumWidth;
          if (nextpage.CurrentPos <= page.GetCurrentPos) then
            page.CurrentPos := nextpage.CurrentPos
          else if (nextpage.CurrentPos >= page.GetCurrentPos + page.Width)  then
            page.Left := nextpage.Left - page.Width
          else
            page.Left := page.GetCurrentPos;

          page.FLeftTemp := page.Left;
          nextpage.FLeftTemp := nextpage.Left;
          page.CurrentPos := page.Left;
          nextpage.CurrentPos := nextpage.CurrentPos;
        end;
      end;
      FUpdateFormBounds := True;
      UpdateFormSize;
    end;
  end;
end;

procedure TAdvSmoothPage.WMNCHitTest(var Message: TWMNCHitTest);
begin
  inherited;
  if not (csDesigning in ComponentState) and Assigned(PageSlider) then
  begin
    if FPageSlider.AllowPageDrag and (FPageSlider.ActivePageIndex = PageIndex) then
      Message.Result := HTCAPTION;
  end;
end;

procedure TAdvSmoothPage.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TAdvSmoothPageSlider then
    PageSlider := TAdvSmoothPageSlider(Reader.Parent);
end;

procedure TAdvSmoothPage.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

procedure TAdvSmoothPage.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothPage.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

procedure TAdvSmoothPage.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  FUpdateFormBounds := True;
  UpdateFormSize;
end;

procedure TAdvSmoothPage.SetColorTones(ATones: TColorTones);
begin
  FMetroStyle := True;
  Fill.Color := ATones.background.BrushColor;
  Fill.ColorTo := ATones.background.BrushColor;

  HeaderFill.Color := ATones.Selected.BrushColor;
  HeaderFill.ColorTo := ATones.Selected.BrushColor;
  HeaderFont.Color := ATones.Selected.TextColor;
  HeaderFont.Name := GetMetroFont;
  HeaderFill.BorderColor := ATones.Selected.bordercolor;

  FooterFill.Color := ATones.Selected.BrushColor;
  FooterFill.ColorTo := ATones.Selected.BrushColor;
  FooterFont.Color := ATones.Selected.TextColor;
  FooterFont.Name := GetMetroFont;
  FooterFill.BorderColor := ATones.Selected.bordercolor;
  Fill.BorderColor := HeaderFill.BorderColor;
end;

procedure TAdvSmoothPage.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  // TODO : do color settings here
  FMetroStyle := False;
  case AStyle of
  tsOffice2003Blue:
    begin
      Fill.Color := $00FFD2AF;
      Fill.ColorTo := $00FFD2AF;

      HeaderFill.Color := $D68759;
      HeaderFill.ColorTo := $933803;
      HeaderFont.Color := clWhite;
      HeaderFill.BorderColor := $962D00;

      FooterFill.Color := $D68759;
      FooterFill.ColorTo := $933803;
      FooterFont.Color := clWhite;
      FooterFill.BorderColor := $962D00;
    end;
  tsOffice2003Silver:
    begin
      Fill.Color := $00E6D8D8;
      Fill.ColorTo := $00E6D8D8;

      HeaderFill.Color := $BDA4A5;
      HeaderFill.ColorTo := $957475;
      HeaderFont.Color := clWhite;
      HeaderFill.BorderColor := $947C7C;

      FooterFill.Color := $BDA4A5;
      FooterFill.ColorTo := $957475;
      FooterFont.Color := clWhite;
      FooterFill.BorderColor := $947C7C;
    end;
  tsOffice2003Olive:
    begin
      Fill.Color := $CFF0EA;
      Fill.ColorTo := $CFF0EA;

      HeaderFill.Color := $82C0AF;
      HeaderFill.ColorTo := $447A63;
      HeaderFont.Color := clWhite;
      HeaderFill.BorderColor := $588060;

      FooterFill.Color := $82C0AF;
      FooterFill.ColorTo := $447A63;
      FooterFont.Color := clWhite;
      FooterFill.BorderColor := $588060;
    end;
    tsOffice2003Classic:
    begin
      Fill.Color := $00F2F2F2;
      Fill.ColorTo := $00F2F2F2;

      HeaderFill.Color := $808080;
      HeaderFill.ColorTo := $808080;
      HeaderFont.Color := clWhite;
      HeaderFill.BorderColor := $808080;

      FooterFill.Color := $808080;
      FooterFill.ColorTo := $808080;
      FooterFont.Color := clWhite;
      FooterFill.BorderColor := $808080;
    end;
  tsOffice2007Luna:
    begin
      Fill.Color := $DCB698;
      Fill.ColorTo := $DCB698;

      HeaderFill.Color := $FFEFE3;
      HeaderFill.ColorTo := $FFD2AF;
      HeaderFont.Color := $723708;
      HeaderFill.BorderColor := $00FFD2AF;

      FooterFill.Color := $FFEFE3;
      FooterFill.ColorTo := $FFD2AF;
      FooterFont.Color := $723708;
      FooterFill.BorderColor := $00FFD2AF;

    end;
  tsOffice2007Obsidian:
    begin
      Fill.Color := $5C534C;
      Fill.ColorTo := $5C534C;

      HeaderFill.Color := $F2F1F0;
      HeaderFill.ColorTo := $C9C2BD;
      HeaderFont.Color := $433C37;
      HeaderFill.BorderColor := $5C534C;

      FooterFill.Color := $F2F1F0;
      FooterFill.ColorTo := $C9C2BD;
      FooterFont.Color := $433C37;
      FooterFill.BorderColor := $5C534C;
    end;
  tsWindowsXP:
    begin
      Fill.Color := $00B6B6B6;
      Fill.ColorTo := $00B6B6B6;

      HeaderFill.Color := clBtnFace;
      HeaderFill.ColorTo := clBtnFace;
      HeaderFont.Color := clBlack;
      HeaderFill.BorderColor := clBlack;

      FooterFill.Color := clBtnFace;
      FooterFill.ColorTo := clBtnFace;
      FooterFont.Color := clBlack;
      FooterFill.BorderColor := clBlack;
    end;
  tsWhidbey:
    begin
      Fill.Color := $F5F9FA;
      Fill.ColorTo := $F5F9FA;

      HeaderFill.Color := $EBEEEF;
      HeaderFill.ColorTo := $7E9898;
      HeaderFont.Color := clWhite;
      HeaderFill.BorderColor := $962D00;

      FooterFill.Color := $EBEEEF;
      FooterFill.ColorTo := $7E9898;
      FooterFont.Color := clWhite;
      FooterFill.BorderColor := $962D00;


    end;
  tsCustom: ;
  tsOffice2007Silver:
    begin
      Fill.Color := $00CAC1BA;
      Fill.ColorTo := $00CAC1BA;

      HeaderFill.Color := $F8F7F6;
      HeaderFill.ColorTo := $E8E0DB;
      HeaderFont.Color := $8B4215;
      HeaderFill.BorderColor := $74706F;

      FooterFill.Color := $F8F7F6;
      FooterFill.ColorTo := $E8E0DB;
      FooterFont.Color := $8B4215;
      FooterFill.BorderColor := $74706F;


    end;
  tsWindowsVista:
    begin
      Fill.Color := $F7EED9;
      Fill.ColorTo := clNone;

      HeaderFill.Color := $FCF9F2;
      HeaderFill.ColorTo := $F7EED9;
      HeaderFont.Color := clBlack;
      HeaderFill.BorderColor := $F9D996;

      FooterFill.Color := $FCF9F2;
      FooterFill.ColorTo := $F7EED9;
      FooterFont.Color := clBlack;
      FooterFill.BorderColor := $F9D996;


    end;
    tsWindows7:
    begin
      Fill.Color := $F7EED9;
      Fill.ColorTo := clNone;

      HeaderFill.Color := $FCF9F2;
      HeaderFill.ColorTo := $F7EED9;
      HeaderFont.Color := clBlack;
      HeaderFill.BorderColor := $F9D996;

      FooterFill.Color := $FCF9F2;
      FooterFill.ColorTo := $F7EED9;
      FooterFont.Color := clBlack;
      FooterFill.BorderColor := $F9D996;


    end;
    tsTerminal:
    begin
      Fill.Color := clWhite;
      Fill.ColorTo := clWhite;

      HeaderFill.Color := clBtnFace;
      HeaderFill.ColorTo := clBtnFace;
      HeaderFont.Color := clBlack;
      HeaderFill.BorderColor := clGray;

      FooterFill.Color := clBtnFace;
      FooterFill.ColorTo := clBtnFace;
      FooterFont.Color := clBlack;
      FooterFill.BorderColor := clGray;


    end;
    tsOffice2010Blue:
    begin
      Fill.Color := $EAD3BF;
      Fill.ColorTo := clNone;

      HeaderFill.Color := $FDF6EF;
      HeaderFill.ColorTo := $F0DAC7;
      HeaderFont.Color := $5B391E;
      HeaderFill.BorderColor := $C7B29F;

      FooterFill.Color := $FDF6EF;
      FooterFill.ColorTo := $F0DAC7;
      FooterFont.Color := $5B391E;
      FooterFill.BorderColor := $C7B29F;



    end;
    tsOffice2010Silver:
    begin
      Fill.Color := $D4CFCB;
      Fill.ColorTo := clNone;

      HeaderFill.Color := $FFFFFF;
      HeaderFill.ColorTo := $EDE5E0;
      HeaderFont.Color := $5B391E;
      HeaderFill.BorderColor := $D2CDC8;

      FooterFill.Color := $FFFFFF;
      FooterFill.ColorTo := $EDE5E0;
      FooterFont.Color := $5B391E;
      FooterFill.BorderColor := $D2CDC8;


    end;
    tsOffice2010Black:
    begin
      Fill.Color := $656565;
      Fill.ColorTo := clNone;

      HeaderFill.Color := $BFBFBF;
      HeaderFill.ColorTo := $919191;
      HeaderFont.Color := $D7D7D6;
      HeaderFill.BorderColor := $6D6D6D;

      FooterFill.Color := $BFBFBF;
      FooterFill.ColorTo := $919191;
      FooterFont.Color := $D7D7D6;
      FooterFill.BorderColor := $6D6D6D;
    end;
  tsWindows8, tsWindows10:
    begin
      Fill.Color := clWhite;
      Fill.ColorTo := clNone;

      HeaderFill.Color := $F7F6F5;
      HeaderFill.ColorTo := $F7F6F5;
      HeaderFont.Color := clBlack;
      HeaderFill.BorderColor := $E4E3E2;

      FooterFill.Color := $F7F6F5;
      FooterFill.ColorTo := $F7F6F5;
      FooterFont.Color := clBlack;
      FooterFill.BorderColor := $E4E3E2;
    end;

  tsOffice2013White:
    begin
      Fill.Color := clWhite;
      Fill.ColorTo := clNone;

      HeaderFill.Color := clWhite;
      HeaderFill.ColorTo := clNone;
      HeaderFont.Color := clBlack;
      HeaderFill.BorderColor := $D4D4D4;

      FooterFill.Color := clWhite;
      FooterFill.ColorTo := clNone;
      FooterFont.Color := clBlack;
      FooterFill.BorderColor := $D4D4D4;
    end;

  tsOffice2013LightGray:
    begin
      Fill.Color := clWhite;
      Fill.ColorTo := clNone;

      HeaderFill.Color := $F6F6F6;
      HeaderFill.ColorTo := clNone;
      HeaderFont.Color := clBlack;
      HeaderFill.BorderColor := $C6C6C6;

      FooterFill.Color := $F6F6F6;
      FooterFill.ColorTo := clNone;
      FooterFont.Color := clBlack;
      FooterFill.BorderColor := $C6C6C6;
    end;

  tsOffice2013Gray:
    begin
      Fill.Color := clWhite;
      Fill.ColorTo := clNone;

      HeaderFill.Color := $E5E5E5;
      HeaderFill.ColorTo := clNone;
      HeaderFont.Color := clBlack;
      HeaderFill.BorderColor := $ABABAB;

      FooterFill.Color := $E5E5E5;
      FooterFill.ColorTo := clNone;
      FooterFont.Color := clBlack;
      FooterFill.BorderColor := $ABABAB;
     end;
  tsOffice2016White:
    begin

      Fill.Color := clWhite;
      Fill.ColorTo := clNone;

      HeaderFill.Color := clWhite;
      HeaderFill.ColorTo := clNone;
      HeaderFont.Color := clBlack;
      HeaderFill.BorderColor := $D4D4D4;

      FooterFill.Color := clWhite;
      FooterFill.ColorTo := clNone;
      FooterFont.Color := clBlack;
      FooterFill.BorderColor := $D4D4D4;

    end;

  tsOffice2016Gray:
    begin
      Fill.Color := $D4D4D4;
      Fill.ColorTo := clNone;

      HeaderFill.Color := $444444;
      HeaderFill.ColorTo := clNone;
      HeaderFont.Color := clBlack;
      HeaderFill.BorderColor := $444444;

      FooterFill.Color := $444444;
      FooterFill.ColorTo := clNone;
      FooterFont.Color := clBlack;
      FooterFill.BorderColor := $444444;
    end;

  tsOffice2016Black:
    begin
      Fill.Color := $363636;
      Fill.ColorTo := clNone;

      HeaderFill.Color := $444444;
      HeaderFill.ColorTo := clNone;
      HeaderFont.Color := $FFFFFF;
      HeaderFill.BorderColor := $444444;

      FooterFill.Color := $444444;
      FooterFill.ColorTo := clNone;
      FooterFont.Color := $FFFFFF;
      FooterFill.BorderColor := $444444;
     end;
  end;

  Fill.BorderColor := HeaderFill.BorderColor;
end;

procedure TAdvSmoothPage.SetCurrentPos(const Value: Integer);
var
  r: TRect;
begin
  if Assigned(FPageSlider) then
  begin
    r := FPageSlider.GetPageRect;
    FCurrentPos := Max(r.Left, Min(r.Right, Value));
  end;
end;

procedure TAdvSmoothPage.SetFill(const Value: TGDIPFill);
begin
  if FFill <> Value then
  begin
    FFill.Assign(Value);
    Invalidate;
  end;
end;

procedure TAdvSmoothPage.SetFooter(const Value: String);
begin
  if FFooter <> Value then
  begin
    FFooter := Value;
    Invalidate;
  end;
end;

procedure TAdvSmoothPage.SetFooterFill(const Value: TGDIPFill);
begin
  if FFooterFill <> Value then
  begin
    FFooterFill.Assign(Value);
    Invalidate;
  end;
end;

procedure TAdvSmoothPage.SetFooterFont(const Value: TFont);
begin
  if FFooterFont <> Value then
  begin
    FFooterFont.Assign(Value);
    Invalidate;
  end;
end;

procedure TAdvSmoothPage.SetFooterHeight(const Value: Integer);
begin
  if FFooterHeight <> Value then
  begin
    FFooterHeight := Value;
    Invalidate;
  end;
end;

procedure TAdvSmoothPage.SetFooterHorizontalAlignment(const Value: TAlignment);
begin
  if FFooterHorizontalAlignment <> Value then
  begin
    FFooterHorizontalAlignment := Value;
    Invalidate;
  end;
end;

procedure TAdvSmoothPage.SetFooterVerticalAlignment(
  const Value: TVerticalAlignment);
begin
  if FFooterVerticalAlignment <> Value then
  begin
    FFooterVerticalAlignment := Value;
    Invalidate;
  end;
end;

procedure TAdvSmoothPage.SetHeader(const Value: String);
begin
  if FHeader <> Value then
  begin
    FHeader := Value;
    Invalidate;
  end;
end;

procedure TAdvSmoothPage.SetHeaderFill(const Value: TGDIPFill);
begin
  if FHeaderFill <> Value then
  begin
    FHeaderFill.Assign(Value);
    Invalidate;
  end;
end;

procedure TAdvSmoothPage.SetHeaderFont(const Value: TFont);
begin
  if FHeaderFont <> Value then
  begin
    FHeaderFont.Assign(Value);
    Invalidate;
  end;
end;

procedure TAdvSmoothPage.SetHeaderHeight(const Value: Integer);
begin
  if FHeaderHeight <> Value then
  begin
    FHeaderHeight := Value;
    Invalidate;
  end;
end;

procedure TAdvSmoothPage.SetHeaderHorizontalAlignment(const Value: TAlignment);
begin
  if FHeaderHorizontalAlignment <> Value then
  begin
    FHeaderHorizontalAlignment := Value;
    Invalidate;
  end;
end;

procedure TAdvSmoothPage.SetHeaderVerticalAlignment(
  const Value: TVerticalAlignment);
begin
  if FHeaderVerticalAlignment <> value then
  begin
    FHeaderVerticalAlignment := Value;
    Invalidate;
  end;
end;

procedure TAdvSmoothPage.SetImageList(const Value: TCustomImageList);
begin
  FImageList := Value;
  Invalidate;
end;

procedure TAdvSmoothPage.SetMinimumWidth(const Value: Integer);
var
  I: Integer;
begin
  if FMinimumWidth <> Value then
  begin
    FMinimumWidth := Value;
    if Assigned(PageSlider) then
    begin
      if not FBlockUpdate then
        PageSlider.SetAllPagesPosition;
    end
    else
    begin
      if Self is TAdvSmoothPageSlider then
      begin
        FBlockUpdate := True;
        for I := 0 to (Self as TAdvSmoothPageSlider).PageCount - 1 do
          (Self as TAdvSmoothPageSlider).Pages[I].MinimumWidth := Value;
        FBlockUpdate := False;

        (Self as TAdvSmoothPageSlider).SetAllPagesPosition;
      end;
    end;
  end;
end;

function TAdvSmoothPage.GetBackGroundRect: TGPRectF;
var
  hr, fr: TGPRectF;
begin
  hr := GetHeaderRect;
  fr := GetFooterRect;
  Result := MakeRect(0, hr.Height, Width - 1, Height - 1 - fr.Height);
end;

function TAdvSmoothPage.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothPage.GetCurrentPos: Integer;
var
  r: TRect;  
begin
  Result := 0;
  if Assigned(FPageSlider) then
  begin
    r := FPageSlider.GetPageRect;
    Result := Max(r.Left, Min(r.Right, FCurrentPos));
  end;
end;

function TAdvSmoothPage.GetFooterRect: TGPRectF;
begin
  Result := MakeRect(0, 0, 0, 0);
  if ShowFooter then
    Result := MakeRect(0, Height - 1 - FooterHeight, Width - 1, FooterHeight);
end;

function TAdvSmoothPage.GetHeaderRect: TGPRectF;
begin
  Result := MakeRect(0, 0, 0, 0);
  if ShowHeader then
    Result := MakeRect(0, 0, Width - 1, HeaderHeight);
end;

function TAdvSmoothPage.GetPageIndex: Integer;
begin
  if Assigned(FPageSlider) then
    Result := FPageSlider.IndexOfPage(Self)
  else
    Result := -1;
end;

function TAdvSmoothPage.GetPosTo: Integer;
begin
  if FPageSlider.ActivePageIndex = PageIndex then
  begin
    if FLeftTemp < MinimumWidth * 2  then    
      Result := MinimumWidth
    else
      Result := MinimumWidth * 2;
  end
  else if FPageSlider.ActivePageIndex > PageIndex then
  begin
    if FLeftTemp < MinimumWidth then    
      Result := 0
    else
      Result := MinimumWidth
  end
  else if FPageSlider.ActivePageIndex < PageIndex then
    Result := FPageSlider.GetPageRect.Right
  else
    Result := GetCurrentPos;
end;

procedure TAdvSmoothPage.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FDownX := X;
  FDownY := Y;
end;

procedure TAdvSmoothPage.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TAdvSmoothPage.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if Assigned(FPageSlider) then
  begin
    if (X >= FDownX - 2) and (X <= FDownX + 1) and (Y >= FDownY - 2) and (Y <= FDownY + 2) then
      FPageSlider.ActivePageIndex := PageIndex;
  end;
end;

procedure TAdvSmoothPage.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if not (csDestroying in ComponentState) then
  begin
    if (Operation = opRemove) and (AComponent = FImageList) then
      FImageList := nil;

    if (Operation = opRemove) and (AComponent = FPictureContainer) then
      FPictureContainer := nil;
  end;
  inherited;
end;

procedure TAdvSmoothPage.SetPageIndex(const Value: Integer);
begin
  if Assigned(FPageSlider) and (Value >= 0) and (Value < FPageSlider.PageCount) then
  begin
    FPageSlider.MovePage(FPageSlider.IndexOfPage(Self), Value);
    FPageSlider.Invalidate;
    Invalidate;
  end;
end;

{ TAdvSmoothPageSlider }

constructor TAdvSmoothPageSlider.Create(AOwner: TComponent);
begin
  FConstructed := false;
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls] - [csOpaque];

  FPages := TDbgList.Create;

  FActivePageIndex := -1;
  FAnimationFactor := 5;
  Height := 400;
  Width := 600;

  FShowFocus := true;
  TabStop := true;
  FAllowPageDrag := True;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 10;
  FTimer.OnTimer := DoTimer;
  FTimer.Enabled := True;

  FDesignTime := (csDesigning in ComponentState) and not
      ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
  begin
    //
  end;
end;

procedure TAdvSmoothPageSlider.CreateWnd;
var
  p: TWinControl;
  t: TAdvSmoothPage;
  gotpages: boolean;
  i: integer;
begin
  inherited;

  if FConstructed then
    Exit;

  gotpages := false;

  if not (csDesigning in ComponentState) then
  begin
    p := self;

    repeat
      p := p.Parent;
    until (p is TForm) or (p is TActiveForm) or not Assigned(p);

    if Assigned(p) then
    begin
      for i := 0 to p.ComponentCount - 1 do
      begin
        if p.Components[i].Name = Name + '1' then
          gotpages := true;
      end;
    end;

  end;

  if FDesignTime and (Name <> '') and not gotpages then
  begin
    t := TAdvSmoothPage.Create(Owner);
    t.PageSlider := self;
    t.Name := Name + '1';
    t.Header := 'Page 1';
    t := TAdvSmoothPage.Create(Owner);
    t.PageSlider := self;
    t.Name := Name + '2';
    t.Header := 'Page 2';
    t := TAdvSmoothPage.Create(Owner);
    t.PageSlider := self;
    t.Name := Name + '3';
    t.Header := 'Page 3';
    ActivePageIndex := 0;
  end;

  FConstructed := true;
end;

destructor TAdvSmoothPageSlider.Destroy;
var
  i: Integer;
begin
  FormHookDone;
  for I := 0 to FPages.Count - 1 do
    TAdvSmoothPage(FPages[I]).FPageSlider := nil;

  FPages.Free;
  FTimer.Free;
  inherited;
end;

procedure TAdvSmoothPageSlider.DoEnter;
begin
  inherited;
  FFocused := true;
  Changed;
end;

procedure TAdvSmoothPageSlider.DoExit;
begin
  inherited;
  FFocused := false;
  Changed;
end;

procedure TAdvSmoothPageSlider.DoTimer(Sender: TObject);
var
  d: Double;
  posto, pos: integer;
  I, c: Integer;
  aPage: TAdvSmoothPage;
  b, anim: Boolean;
begin
  if (csDesigning in ComponentState) or (csDestroying in ComponentState) then
    Exit;

  b := IsWindowVisible(Handle);
  for I := 0 to PageCount - 1 do
  begin
    if Assigned(Pages[I].FShadowForm) then
      Pages[I].FShadowForm.Visible := b;
  end;

  if Assigned(FShadowForm) then
    FShadowForm.Visible := b;

  if FAnimate then
  begin
    anim := False;
    for I := 0 to PageCount - 1 do
    begin
      aPage := TAdvSmoothPage(Pages[I]);
      posTo := aPage.GetPosTo;
      d := Abs(posto - aPage.GetCurrentPos) / AnimationFactor;
      pos := aPage.CurrentPos;
      aPage.FAnimating := AnimateDouble(pos, posto, d, 1);
      aPage.CurrentPos := pos;
      anim := anim or aPage.FAnimating;
      if not apage.FAnimating then
        apage.FLeftTemp := 0
    end;
    FAnimate := anim;

    if not FAnimate then
    begin
      if Assigned(OnPageChanged) then
        OnPageChanged(Self, ActivePageIndex);

      for I := 0 to pageCount - 1 do
      begin
        for c := 0 to Pages[I].ControlCount - 1 do
          Pages[I].Controls[c].Invalidate;
      end;
    end;
    SetAllPagesPosition;
  end;
end;

procedure TAdvSmoothPageSlider.MovePage(CurIndex, NewIndex: Integer);
var
  OldActivePage: TAdvSmoothPage;
begin
  if (CurIndex >= 0) and (CurIndex < FPages.Count) and
     (NewIndex >= 0) and (NewIndex < FPages.Count) then
  begin
    OldActivePage := ActivePage;
    FPages.Move(CurIndex, NewIndex);
    ActivePage := OldActivePage;

    if Assigned(FOnPageMoved) then
      FOnPageMoved(Self, CurIndex, NewIndex);
  end;
end;

function TAdvSmoothPageSlider.AddPage(APage: TAdvSmoothPage): integer;
begin
  Result := FPages.IndexOf(APage);
  if (FPages.IndexOf(APage) < 0) then
  begin
    FPages.Add(APage);
    APage.FPageIndex := FPages.Count - 1;
    Result := FPages.Count - 1;
  end;

  if (APage.Parent <> Self) then
    APage.Parent := Self;
  APage.FPageSlider := Self;
  SetAllPagesPosition;

  if Assigned(ActivePage) then
  begin
    ActivePage.BringToFront;
    ActivePage.Invalidate;
  end;
end;

function TAdvSmoothPageSlider.AddPage(PageCaption: TCaption): integer;
var
  aPage: TAdvSmoothPage;
begin
  aPage := TAdvSmoothPage.Create(Self);
  aPage.Caption := PageCaption;
  aPage.FPageIndex := FPages.Count - 1;
  Result := AddPage(aPage);
end;

procedure TAdvSmoothPageSlider.AdjustClientRect(var Rect: TRect);
begin
  inherited;
end;

procedure TAdvSmoothPageSlider.AlignControls(AControl: TControl;
  var ARect: TRect);
begin
  ARect.Bottom := ARect.Bottom - Round(GetFooterRect.Height) - HeaderFill.BorderWidth;
  ARect.Top := ARect.Top + Round(GetHeaderRect.Height) + HeaderFill.BorderWidth;
  ARect.Right := MinimumWidth;
  ARect.Left := Fill.BorderWidth;
  inherited AlignControls(AControl, ARect);
  SetAllPagesPosition;
end;

procedure TAdvSmoothPageSlider.Loaded;
begin
  inherited;
  FPropertiesLoaded := True;
end;

procedure TAdvSmoothPageSlider.NextPage;
begin
  ActivePageIndex := ActivePageIndex + 1;
end;

procedure TAdvSmoothPageSlider.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if not (csDestroying in ComponentState) and (Operation = opRemove) then
  begin
    if (AComponent = PopupMenu) then
      PopupMenu := nil;
  end;

  inherited;
end;

procedure TAdvSmoothPageSlider.Paint;
var
  th: integer;
begin
  inherited;

  if (csDesigning in ComponentState) and (FPages.Count = 0) then
  begin
    Canvas.Font.Assign(self.Font);
    Canvas.Brush.Style := bsClear;
    th := Canvas.TextHeight('gh');
    Canvas.TextOut(10, Height div 2, 'Right-click and choose "New Page"');
    Canvas.TextOut(10, (Height div 2) + th,'to insert a new page');
    Canvas.Font.Style := [fsItalic];
    Canvas.TextOut(10, Height div 2 + 3*th, 'If no such right-click menu option appears');
    Canvas.TextOut(10, Height div 2 + 4*th, 'please install designtime package!');
  end;
end;

procedure TAdvSmoothPageSlider.PreviousPage;
begin
  ActivePageIndex := ActivePageIndex - 1;
end;

procedure TAdvSmoothPageSlider.RemovePage(APage: TAdvSmoothPage);
var
  i, ni: Integer;
begin
  i := FPages.IndexOf(APage);
  if (i >= 0) then
  begin
    if i < ActivePageIndex then
      ni := ActivePageIndex - 1
    else
      ni := ActivePageIndex;

    if (ActivePage = APage) then
      SelectNextPage(True);

    FPages.Delete(i);
    APage.FPageSlider := nil;

    ActivePageIndex := ni;
    Invalidate;
    if Assigned(ActivePage) then
      ActivePage.Invalidate;
  end;
end;

procedure TAdvSmoothPageSlider.Resize;
var
  i: Integer;
begin
  inherited;
  for I := 0 to pageCount - 1 do
    TAdvSmoothPage(Pages[I]).CurrentPos := TAdvSmoothPage(Pages[I]).GetPosTo;
    
  SetAllPagesPosition;

  for I := 0 to pageCount - 1 do
  begin
    TAdvSmoothPage(Pages[I]).FUpdateFormBounds := True;
    TAdvSmoothPage(Pages[I]).UpdateFormSize;
    if Assigned(TAdvSmoothPage(Pages[I]).FShadowForm) then    
      TAdvSmoothPage(Pages[I]).FShadowForm.Invalidate;
  end;
end;

procedure TAdvSmoothPageSlider.SetParent(AParent: TWinControl);
begin
  if (AParent is TAdvSmoothPageSlider) then
    raise Exception.Create('Invalid Parent');

  inherited;

  if (not FPropertiesLoaded) and not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    Init;
end;

function TAdvSmoothPageSlider.GetPageCount: integer;
begin
  Result := FPages.Count;
end;


function TAdvSmoothPageSlider.GetPage(index: integer): TAdvSmoothPage;
begin
  Result := TAdvSmoothPage(FPages[index]);
end;

procedure TAdvSmoothPageSlider.SetShowFocus(const Value: Boolean);
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPageSlider.SetColorTones(ATones: TColorTones);
var
  i: integer;
begin
  inherited;
  for I := 0 to FPages.Count - 1 do
    Pages[I].SetColorTones(ATones);
end;

procedure TAdvSmoothPageSlider.SetComponentStyle(AStyle: TTMSStyle);
var
  i: Integer;
begin
  inherited;
  for I := 0 to FPages.Count - 1 do
     Pages[I].SetComponentStyle(AStyle);
end;

procedure TAdvSmoothPageSlider.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited;
  if TabStop and not (csDesigning in ComponentState) then
    SetFocus;

  P := Point(X, Y);
end;

procedure TAdvSmoothPageSlider.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  P := Point(X, Y);
end;

procedure TAdvSmoothPageSlider.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited;
  P := Point(X, Y);
end;

procedure TAdvSmoothPageSlider.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
var
  I: Integer;
  Control: TControl;
begin
  for I := 0 to FPages.Count - 1 do Proc(TComponent(FPages[I]));

  for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];
    if (Control.Owner = Root) and (FPages.IndexOf(Control) < 0) then Proc(Control);
  end;
end;

function TAdvSmoothPageSlider.FindNextPage(CurPage: TAdvSmoothPage; GoForward: Boolean): TAdvSmoothPage;
var
  i, CurIndex: Integer;
begin
  Result := nil;
  CurIndex := FPages.IndexOf(CurPage);


  if (CurPage = nil) or (CurIndex < 0) then
  begin

    if FPages.Count > 0 then
    begin
      if GoForward then
        Result := FPages[0]
      else
        Result := FPages[FPages.Count - 1];
    end;
    Exit;
  end;

  if GoForward then
  begin
    i := CurIndex;
    Inc(i);
    if (i >= FPages.Count) then
      i := 0;

    Result := Pages[i];
  end
  else  // BackWard
  begin
    i := CurIndex;
    dec(i);
    if (i >= FPages.Count) then
      i := 0;
    if (i < 0) then
      i := FPages.Count-1;

    Result := Pages[i];
  end;
end;

function TAdvSmoothPageSlider.GetActivePage: TAdvSmoothPage;
begin
  Result := nil;
  if (ActivePageIndex >= 0) and (ActivePageIndex < FPages.Count) then
    Result := Pages[FActivePageIndex];
end;

function TAdvSmoothPageSlider.GetActivePageIndex: Integer;
begin
  Result := FActivePageIndex;
end;

procedure TAdvSmoothPageSlider.SelectNextPage(GoForward: Boolean);
begin
  if GoForward then
    NextPage
  else
    PreviousPage;
end;

function TAdvSmoothPageSlider.IndexOfPage(APage: TAdvSmoothPage): Integer;
begin
  Result := FPages.IndexOf(APage);
end;

procedure TAdvSmoothPageSlider.SetActivePage(const Value: TAdvSmoothPage);
begin
  if (FPages.IndexOf(Value) >= 0) then
    ActivePageIndex := FPages.IndexOf(Value);
end;

procedure TAdvSmoothPageSlider.ChangeActivePage(PageIndex: Integer);
var
  aForm: TCustomForm;
  AllowChange: Boolean;
  I: Integer;
  b: Boolean;
begin
  if (PageIndex >= -1) and (PageIndex < FPages.Count) then
  begin
    AllowChange := True;
    if Assigned(FOnChanging) and FPropertiesLoaded and not (csDestroying in ComponentState) then
      FOnChanging(Self, ActivePageIndex, PageIndex, AllowChange);

    if not AllowChange then
      Exit;


    b := FActivePageIndex < PageIndex;

    FActivePageIndex := PageIndex;

    if Assigned(FOnChange) and not (csDestroying in ComponentState)
      and not (csLoading in ComponentState) then
      FOnChange(Self);

    if Assigned(FOnPageChange) and not (csDestroying in ComponentState)
      and not (csLoading in ComponentState) then
      FOnPageChange(Self);

    if FActivePageIndex > -1 then
    begin
      if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      begin
        aForm := GetParentForm(Self);
        if (aForm <> nil) and (aForm.Designer <> nil) then
          aForm.Designer.Modified;
      end;

      if b then
      begin
        for I := 0 to GetPageCount - 1 do
        begin
          if (csDesigning in ComponentState) and (TAdvSmoothPage(FPages[I]).PageIndex <> PageIndex) then
            TAdvSmoothPage(FPages[PageIndex]).Visible := False
          else
          begin
            if not  TAdvSmoothPage(FPages[PageIndex]).Visible then
              TAdvSmoothPage(FPages[PageIndex]).Visible := True;
          end;
        end;
      end;
    end;
  end;
  
  SetAllPagesPosition;
end;

procedure TAdvSmoothPageSlider.SetActivePageIndex(const Value: Integer);
begin
  ChangeActivePage(Value);
  if csDesigning in ComponentState then
  begin
    if Assigned(ActivePage) then
      ActivePage.BringToFront;
  end;
  FAnimate := True;
  Invalidate;
end;

procedure TAdvSmoothPageSlider.SetAllPagesPosition;
var
  i: Integer;
  R: TRect;
  APage: TAdvSmoothPage;
begin
  R := GetPageRect;
  for i := 0 to FPages.Count - 1 do
  begin
    APage := TAdvSmoothPage(FPages[i]);
    if (APage <> nil) and (FPages.IndexOf(APage) >= 0) then
    begin
      if csDesigning in ComponentState then
      begin
        APage.SetBounds(r.Left + APage.MinimumWidth, R.Top, R.Right - r.Left - APage.MinimumWidth,
          R.Bottom - R.Top);
      end
      else
      begin
        if (csLoading in ComponentState) then
          APage.CurrentPos := r.Right;

        APage.SetBounds(r.Left + APage.GetCurrentPos, R.Top, R.Right - r.Left - APage.MinimumWidth,
          R.Bottom - R.Top);
      end;
    end;
  end;
end;

procedure TAdvSmoothPageSlider.SetAnimationFactor(const Value: Integer);
begin
  if FAnimationFactor <> Value then
    FAnimationFactor := Max(1, Value);
end;

function TAdvSmoothPageSlider.GetPageRect: TRect;
begin
  Result := ClientRect;
end;

procedure TAdvSmoothPageSlider.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
  if (csDesigning in ComponentState) then
  begin
  end;
  inherited;
end;

function TAdvSmoothPageSlider.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

procedure TAdvSmoothPageSlider.SetVersion(const Value: string);
begin

end;

function TAdvSmoothPageSlider.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvSmoothPageSlider.CMDialogChar(var Message: TCMDialogChar);
var
  I: Integer;
begin
  for I:= 0 to FPages.Count-1 do
    if IsAccel(Message.CharCode, Pages[I].Caption) and CanFocus then
    begin
      Message.Result := 1;
      ActivePageIndex := I;
      Exit;
    end;
  inherited;
end;

procedure TAdvSmoothPageSlider.Init;
begin
  FPropertiesLoaded := true;
end;

procedure TAdvSmoothPageSlider.WMKeyDown(var Message: TWMKeyDown);
var
  Ctrl: TWinControl;
begin
  case Message.CharCode of
    VK_HOME: ActivePageIndex := 0;
    VK_END: ActivePageIndex := PageCount - 1;
    VK_LEFT, VK_UP:
    begin
      SelectNextPage(False);
    end;
    VK_RIGHT, VK_DOWN:
    begin
      SelectNextPage(True);
    end;
    VK_TAB:
    begin
      if Assigned(Self.Parent) then
      begin
        Ctrl := TProWinControl(Self.Parent).FindNextControl(Self, True, True, True);
        if Assigned(Ctrl) and Ctrl.CanFocus then
        begin
          Ctrl.SetFocus;
        end;
      end;
    end;
  end;
  inherited;
end;

procedure TAdvSmoothPageSlider.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS + DLGC_WANTCHARS;
end;

procedure TAdvSmoothPageSlider.WndProc(var Msg: TMessage);
var
  p: TWinControl;
begin
  if (Msg.Msg = WM_DESTROY) then
  begin
    if not (csDesigning in ComponentState) and Assigned(FFormWndProc) then
    begin
      p := self;
      repeat
        p := p.Parent;
      until (p is TForm) or (p is TActiveForm) or not Assigned(p);

      if (p <> nil) then
      begin
        p.WindowProc := FFormWndProc;
        FFormWndProc := nil;
      end;
    end;
  end;

  inherited;
end;


function TAdvSmoothPageSlider.IsActivePageNeighbour(
  PageIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  if (PageIndex = ActivePageIndex) or (PageIndex < 0) or (PageIndex >= PageCount) then
    Exit;

  if (PageIndex < ActivePageIndex) then
  begin
    for i:= ActivePageIndex - 1 downto PageIndex do
    begin
      if (i = PageIndex) then
        Result := -1;
      Break;
    end;
  end
  else
  begin
    for i:= ActivePageIndex + 1 to PageIndex do
    begin
      if (i = PageIndex) then
        Result := 1;
      Break;
    end;
  end;
end;

{ TAdvSmoothPageShadow }

procedure TAdvSmoothPageShadow.ClearBuffer(graphics: TGPGraphics);
var
  g: TGPGraphics;
begin
  g := graphics;
  if not Assigned(g) then
    g := CreateGraphics;
  g.Clear($00000000);
  if not Assigned(graphics) then
    g.Free;
end;

procedure TAdvSmoothPageShadow.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
end;

function TAdvSmoothPageShadow.CreateGraphics: TGPGraphics;
begin
  Result := nil;
  if Assigned(FMainBuffer) then
    Result := TGPGraphics.Create(FMainBuffer);
end;

procedure TAdvSmoothPageShadow.CreateMainBuffer;
begin
  if Assigned(FMainBuffer) then
  begin
    FMainBuffer.Free;
    FMainBuffer := nil;
  end;

  FMainBuffer := TGPBitmap.Create(Width, Height, PixelFormat32bppARGB);
end;

procedure TAdvSmoothPageShadow.CreateWnd;
begin
  inherited;
  if Assigned(FPage) then
    FPage.FormHookInit;
  UpdateWindow;
end;

procedure TAdvSmoothPageShadow.DestroyMainBuffer;
begin
  if Assigned(FMainBuffer) then
  begin
    FMainBuffer.Free;
    FMainBuffer := nil;
  end;
end;

procedure TAdvSmoothPageShadow.DoCreate;
begin
  inherited;
  FMainBuffer := nil;
end;

procedure TAdvSmoothPageShadow.DoDestroy;
begin
  inherited;
  DestroyMainBuffer;
end;

procedure TAdvSmoothPageShadow.Draw(graphics: TGPGraphics);
var
  g: TGPGraphics;
  lb: TGPLinearGradientBrush;
  r: TGPRectF;
  c: TColor;
  cb: array[0..2] of TGPColor;
  pb: array[0..2] of Single;
begin
  g := graphics;
  if not Assigned(g) then
    g := CreateGraphics;

  g.SetSmoothingMode(SmoothingModeAntiAlias);
  g.SetTextRenderingHint(TextRenderingHintAntiAlias);

  if Assigned(OwnerPage) then
  begin
    c := OwnerPage.ShadowColor;
    if c <> clNone then
    begin
      r := MakeRect(Width - w, 0, w - 1, Height - 1);
      lb := TGPLinearGradientBrush.Create(MakeRect(r.X - 1, r.Y - 1, r.Width + 2, r.Height + 2), MakeColor(0, c), MakeColor(0, c), LinearGradientModeHorizontal);
      cb[0] := MakeColor(0, c);
      cb[1] := MakeColor(40, c);
      cb[2] := MakeColor(100, c);

      pb[0] := 0;
      pb[1] := 0.8;
      pb[2] := 1;

      lb.SetInterpolationColors(@cb, @pb, 3);
      g.FillRectangle(lb, r);
      lb.free;
    end;
  end;

  if not Assigned(graphics) then
    g.Free;
end;

procedure TAdvSmoothPageShadow.FormHookDone;
var
  f: TCustomForm;
begin
  if Assigned(FPage) then
  begin
    f := GetParentForm(FPage);
    if Assigned(f) and f.HandleAllocated then
    {$IFDEF DELPHI_UNICODE}
      SetWindowLongPtr(f.Handle, GWL_WNDPROC, LInteger(OldWndProc));
    {$ENDIF}
    {$IFNDEF DELPHI_UNICODE}
      SetWindowLong(f.Handle, GWL_WNDPROC, LInteger(OldWndProc));
    {$ENDIF}
  end;
end;

procedure TAdvSmoothPageShadow.FormHookInit;
var
  f: TCustomForm;
begin
  if Assigned(FPage) then
  begin
    f := GetParentForm(FPage);
    if assigned(f) then
    begin
       { Hook parent }

      {$IFDEF DELPHI_UNICODE}
      OldWndProc := TFarProc(GetWindowLongPtr(f.Handle, GWL_WNDPROC));
      {$ENDIF}
      {$IFNDEF DELPHI_UNICODE}
      OldWndProc := TFarProc(GetWindowLong(f.Handle, GWL_WNDPROC));
      {$ENDIF}

      {$IFDEF DELPHI9_LVL}
      NewWndProc := Classes.MakeObjectInstance(HookWndProc);
      {$ELSE}
      NewWndProc := MakeObjectInstance(HookWndProc);
      {$ENDIF}

      {$IFDEF DELPHI_UNICODE}
      SetWindowLongPtr(f.Handle, GWL_WNDPROC, LInteger(NewWndProc));
      {$ENDIF}
      {$IFNDEF DELPHI_UNICODE}
      SetWindowLong(f.Handle, GWL_WNDPROC, LInteger(NewWndProc));
      {$ENDIF}
    end;
  end;
end;


procedure TAdvSmoothPageShadow.HookWndProc(var Msg: TMessage);
var
  f: TCustomForm;
begin
  if csDestroying in ComponentState then
    Exit;

  if Assigned(FPage) then
  begin     
    f := GetParentForm(FPage);
    if Assigned(f) then
    begin
      Msg.Result := CallWindowProc(OldWndProc, f.Handle, Msg.Msg , Msg.wParam, Msg.lParam);
      case Msg.Msg of
       WM_ACTIVATE:
       begin
         if FPage.Visible then
           PostMessage(Self.Handle, WM_USERACTIVATE, MSG.WParam, 0);
       end;
       WM_WINDOWPOSCHANGING:
       begin
         FPage.FUpdateFormBounds := true;
         FPage.UpdateFormSize;
       end;
       WM_SIZE:
       begin
         if not FMouseEntered then
         begin
           FPage.FUpdateFormBounds := true;
           FPage.UpdateFormSize;
           FPage.Changed;
         end;
       end;
      end;
    end;
  end;
end;

procedure TAdvSmoothPageShadow.Init;
begin
  Visible := False;
  BorderIcons := [];
  BorderStyle := bsNone;
  Ctl3D := false;
  FormStyle := fsStayOnTop;
  Color := clWhite;
  Position := poScreenCenter;
  CreateMainBuffer;
  SetLayeredWindow;
  UpdateLayered;
end;

procedure TAdvSmoothPageShadow.Paint;
begin
  inherited;
  UpdateWindow;
end;

procedure TAdvSmoothPageShadow.SetLayeredWindow;
begin
  if GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_LAYERED = 0 then
    SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);

  UpdateLayered;
end;

procedure TAdvSmoothPageShadow.SetW(const Value: Integer);
begin
  if FW <> Value then
  begin
    FW := Value;
    Invalidate;
  end;
end;

procedure TAdvSmoothPageShadow.UpdateLayered;
begin
  ClearBuffer(nil);

  SetWindowPos(Self.Handle, HWND_TOPMOST, 0, 0, 0, 0,
    SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED or SWP_NOACTIVATE);

  Draw(nil);

  UpdateMainWindow;
end;

procedure TAdvSmoothPageShadow.UpdateMainWindow;
var
  ScrDC, MemDC: HDC;
  BitmapHandle, PrevBitmap: HBITMAP;
  BlendFunc: _BLENDFUNCTION;
  Size: TSize;
  P, S: TPoint;
begin
//  while BlendFunc.SourceConstantAlpha < 255 do
//  begin
    ScrDC := CreateCompatibleDC(0);
    MemDC := CreateCompatibleDC(ScrDC);

    FMainBuffer.GetHBITMAP(0, BitmapHandle);
    PrevBitmap := SelectObject(MemDC, BitmapHandle);
    Size.cx := Width;
    Size.cy := Height;
    P := Point(Left, Top);
    S := Point(0, 0);

    with BlendFunc do
    begin
      BlendOp := AC_SRC_OVER;
      BlendFlags := 0;
      SourceConstantAlpha := 255;
      AlphaFormat := AC_SRC_ALPHA;
    end;

    UpdateLayeredWindow(Handle, ScrDC, @P, @Size, MemDC, @S, 0, @BlendFunc, ULW_ALPHA);

    SelectObject(MemDC, PrevBitmap);
    DeleteObject(BitmapHandle);

    DeleteDC(MemDC);
    DeleteDC(ScrDC);
//  end;
end;

procedure TAdvSmoothPageShadow.UpdateWindow;
begin
  CreateMainBuffer;
  UpdateLayered;
end;

procedure TAdvSmoothPageShadow.WMActivate(var Message: TMessage);
begin
  inherited;
  Message.Result := 1;
end;

procedure TAdvSmoothPageShadow.WMMouseActivate(var msg: TWMMouseActivate);
begin
  msg.result := MA_NOACTIVATE;
end;

procedure TAdvSmoothPageShadow.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  DefaultHandler(Msg);
  if Msg.Result = HTCAPTION then
    Msg.Result := HTNOWHERE;
end;

procedure TAdvSmoothPageShadow.WndProc(var Message: TMessage);
begin
  if Assigned(FPage) and not (csDestroying in ComponentState) then
  begin
    if Message.Msg = WM_DESTROY then
      FPage.FormHookDone
    else if Message.Msg = WM_USERACTIVATE then
    begin
      if IsWindowVisible(FPage.Handle) then
      begin
        FPage.FUpdateFormBounds := true;
        FPage.UpdateFormSize;
        UpdateWindow;
        case Message.WParam of
          0: ShowWindow(Self.Handle, SW_HIDE);
          1, 2: ShowWindow(Self.Handle, SW_SHOWNA);
        end;
      end;
    end;
  end;
  inherited;
end;

end.
