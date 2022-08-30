{***************************************************************************}
{ TSPBEPC component                                                         }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2010 - 2012                                        }
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

unit SPBEPC;

{$I TMSDEFS.INC}

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, Graphics, Windows, Math, Forms, Messages
  , Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;


const
  EPC_A_Color : TColor = $005A8400;
  EPC_B_Color : TColor = $0059B019;
  EPC_C_Color : TColor = $0041C68D;
  EPC_D_Color : TColor = $0000CCFF;
  EPC_E_Color : TColor = $0064ACF7;
  EPC_F_Color : TColor = $002185F7;
  EPC_G_Color : TColor = $00391CEF;

  EIC_A_Color : TColor = $00F7CE73;
  EIC_B_Color : TColor = $00FFB521;
  EIC_C_Color : TColor = $00DE9C00;
  EIC_D_Color : TColor = $00C67B00;
  EIC_E_Color : TColor = $00BDBDBD;
  EIC_F_Color : TColor = $009C9C9C;
  EIC_G_Color : TColor = $00848484;


  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0  : First release
  // v1.0.0.1  : Improved : Paint code to allow painting in non visual runtime created


type
  TSPBEPCChartType = (ctEPC, ctEIC);

  TSPBEPCLocationDetails = class
  private
    FPotentialWidth: Integer;
    FATop: Integer;
    FCurrentWidth: Integer;
    FStairSize: Integer;
    FPotentialleft: Integer;
    FCurrentLeft: Integer;
  public
    property CurrentLeft    : Integer read FCurrentLeft    write FCurrentLeft;
    property CurrentWidth   : Integer read FCurrentWidth   write FCurrentWidth;
    property Potentialleft  : Integer read FPotentialleft  write FPotentialleft;
    property PotentialWidth : Integer read FPotentialWidth write FPotentialWidth;
    property StairSize : Integer read FStairSize write FStairSize;
    property ATop : Integer read FATop write FATop;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TSPBEPC = class(TCustomControl)
  private
    BaseImage : TImage;
    FCurrent: Integer;
    FPotential: Integer;
    FTitle : string;
    DoingUpdate : Boolean;
    FCurrentTitle: string;
    FPotentialTitle: string;
    FTopCaption : string;
    FBottomCaption : string;
    FPenWidth: Integer;
    FChartType: TSPBEPCChartType;
    FPotentialVisible: Boolean;
    FCurrentVisible: Boolean;
    FReadOnly: Boolean;
    FMouseDraging: Boolean;
    FMouseSliding: Boolean;
    FSelectedCol: Integer;
    FPicture: TPicture;
    FRepeatTimer: TTimer;
    FSliderDownPt: TPoint;
    FBottomCaptionFont: TFont;
    FTopCaptionFont: TFont;
    FTitleFont: TFont;
    FPotentialTitleAutoSize: Boolean;
    FCurrentTitleAutoSize: Boolean;
    FPenColor: TColor;
    FTitleColor: TColor;
    FSlideDirUp: Integer;
    FOnPotentialChange: TNotifyEvent;
    FOnCurrentChange: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;  // 0 = down; 1 = up; -1 No Dir
    procedure SetCurrent(const Value: Integer);
    procedure SetPotential(const Value: Integer);
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetCurrentTitle: string;
    procedure SetCurrentTitle(const Value: string);
    function GetPotentialTitle: string;
    procedure SetPotentialTitle(const Value: string);
    procedure SetBottomCaption(const Value: string);
    procedure SetTopCaption(const Value: string);
    procedure SetPenWidth(const Value: Integer);
    function GetBottomCaption: string;
    function GetTopCaption: string;
    function GetCurrentColor: TColor;
    function GetPotentialColor: TColor;
    procedure SetChartType(const Value: TSPBEPCChartType);
    procedure SetCurrentVisible(const Value: Boolean);
    procedure SetPotentialVisible(const Value: Boolean);
    function GetColAtPos(P: TPoint): Integer;
    procedure SetSelectedCol(const Value: Integer);
    procedure SetPicture(const Value: TPicture);
    procedure PictureChanged(Sender: TObject);
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    function GetPointerRect(ColIndex: Integer): TRect;   // Current = 2; Potential = 3
    procedure RepeatTimerExpired(Sender: TObject);
    procedure OnFontChanged(Sender: TObject);
    procedure ClickSlider;
    procedure SetBottomCaptionFont(const Value: TFont);
    procedure SetTitleFont(const Value: TFont);
    procedure SetTopCaptionFont(const Value: TFont);
    procedure SetCurrentTitleAutoSize(const Value: Boolean);
    procedure SetPotentialTitleAutoSize(const Value: Boolean);
    procedure SetPenColor(const Value: TColor);
    procedure SetTitleColor(const Value: TColor);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    { Private declarations }
  private
    // Used to pass draw info to save recalculating..
    BaseLocationDetails : TSPBEPCLocationDetails;
    RebuildBaseImage : Boolean;
    //# GetTopPosition is used to calculate the top position for the rect for Current or Potential
    function GetTopPosition(Value : Integer) : Integer;
    function GetValueFromTopPosition(TopPosition: Integer): Integer;
    procedure MakeTextFit(Canvas : TCanvas; var Rect: TRect; Str: string);
    procedure RefreshBaseImage;
    procedure DoOnResize(Sender : TObject);
    property OnResize;
    function GetColorForValue(Value : Integer):TColor;
    function A_Color : TColor;
    function B_Color : TColor;
    function C_Color : TColor;
    function D_Color : TColor;
    function E_Color : TColor;
    function F_Color : TColor;
    function G_Color : TColor;
  protected
    { Protected declarations }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;

    function DestRect: TRect;
    procedure Paint; override;
    function GetCanvas: TCanvas;

    property SelectedCol: Integer read FSelectedCol write SetSelectedCol;
    property Canvas: TCanvas read GetCanvas;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    //# Call BeginUpdate to prevent the image reloading.
    procedure BeginUpdate;
    //# Call EndUpdate to tell the image its allowed to reload after changes.
    procedure EndUpdate;
    //# Call Refresh to repaint the image. This will not execute if between BeginUpdate and EndUpdate
    procedure Refresh;
    procedure SaveToFile(FileName: string);
    property Picture: TPicture read FPicture write SetPicture;
    function GetVersionNr: Integer;
  published
    { Published declarations }
    //# Current Engery Scoring
    property Current : Integer read FCurrent write SetCurrent default 0;
    //# Potential Engery Scoring
    property Potential : Integer read FPotential write SetPotential default 0;
    //# Chart Title - Default is "Energy Efficiency Rating"
    property Title : string read GetTitle write SetTitle;
    //# The first column is "Current"
    property CurrentTitle : string read GetCurrentTitle write SetCurrentTitle;
    //# The second column is "Potential"
    property PotentialTitle : string read GetPotentialTitle write SetPotentialTitle;
    //# Top Caption shows the hint to a high score - default is "Very energy efficient - lower running costs"
    property TopCaption : string read GetTopCaption write SetTopCaption;
    //# Bottom Caption shows the hint to a low score - default is "Not energy efficient - higher running costs"
    property BottomCaption : string read GetBottomCaption write SetBottomCaption;
    //# Pen width is the size of the borders on screen.
    property PenWidth : Integer read FPenWidth write SetPenWidth default 1;
    //# Current Value Color
    property CurrentColor : TColor read GetCurrentColor;
    //# Potential Value Color
    property PotentialColor : TColor read GetPotentialColor;
    //# ChartType
    property ChartType : TSPBEPCChartType read FChartType write SetChartType default ctEPC;

    property CurrentVisible : Boolean read FCurrentVisible write SetCurrentVisible default True;
    property PotentialVisible : Boolean read FPotentialVisible write SetPotentialVisible default True;
    property ReadOnly : Boolean read FReadOnly write FReadOnly default False;
    property BottomCaptionFont: TFont read FBottomCaptionFont write SetBottomCaptionFont;
    property TopCaptionFont: TFont read FTopCaptionFont write SetTopCaptionFont;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    property CurrentTitleAutoSize: Boolean read FCurrentTitleAutoSize write SetCurrentTitleAutoSize default false;
    property PotentialTitleAutoSize: Boolean read FPotentialTitleAutoSize write SetPotentialTitleAutoSize default false;
    property PenColor: TColor read FPenColor write SetPenColor default clBlack;
    property TitleColor: TColor read FTitleColor write SetTitleColor;
    property Version: string read GetVersion write SetVersion;

    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnCurrentChange: TNotifyEvent read FOnCurrentChange write FOnCurrentChange;
    property OnPotentialChange: TNotifyEvent read FOnPotentialChange write FOnPotentialChange;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  Consts, uxtheme;

//------------------------------------------------------------------------------
{$IFNDEF DELPHI2007_LVL}
type
  TTextFormats = (tfBottom, tfCalcRect, tfCenter, tfEditControl, tfEndEllipsis,
    tfPathEllipsis, tfExpandTabs, tfExternalLeading, tfLeft, tfModifyString,
    tfNoClip, tfNoPrefix, tfRight, tfRtlReading, tfSingleLine, tfTop,
    tfVerticalCenter, tfWordBreak);
  TTextFormat = set of TTextFormats;
{$ENDIF}

procedure DrawTextRect(Canvas: TCanvas; var Rect: TRect; var Text: string; TextFormat: TTextFormat = []);
const
  cTextFormats: array[TTextFormats] of Integer =
  (DT_BOTTOM, DT_CALCRECT, DT_CENTER, DT_EDITCONTROL, DT_END_ELLIPSIS,
   DT_PATH_ELLIPSIS, DT_EXPANDTABS, DT_EXTERNALLEADING, DT_LEFT,
   DT_MODIFYSTRING, DT_NOCLIP, DT_NOPREFIX, DT_RIGHT, DT_RTLREADING,
   DT_SINGLELINE, DT_TOP, DT_VCENTER, DT_WORDBREAK
{$IFDEF DELPHIXE2_LVL}
   ,0,0,0,0,0,0
{$ENDIF}
   );
var
  Format: Integer;
  F: TTextFormats;
begin
  Format := 0;
  for F := Low(TTextFormats) to High(TTextFormats) do
    if F in TextFormat then
      Format := Format or cTextFormats[F];

  { Platform SDK:
    "If dwDTFormat includes DT_MODIFYSTRING, the function could add up to four additional characters
    to this string. The buffer containing the string should be large enough to accommodate these
    extra characters." }
  if tfModifyString in TextFormat then
    SetLength(Text, Length(Text) + 5);

  DrawTextEx(Canvas.Handle, PChar(Text), Length(Text), Rect, Format, nil);
  if tfModifyString in TextFormat then
    SetLength(Text, StrLen(PChar(Text)));
end;

//------------------------------------------------------------------------------

{ TSPBEPC }

function TSPBEPC.A_Color : TColor;
begin
  case ChartType of
    ctEPC: Result := EPC_A_Color;
    ctEIC: Result := EIC_A_Color;
    else Result := clBlack;
  end;
end;

//------------------------------------------------------------------------------

function TSPBEPC.B_Color : TColor;
begin
  case ChartType of
    ctEPC: Result := EPC_B_Color;
    ctEIC: Result := EIC_B_Color;
    else Result := clBlack;
  end;
end;

//------------------------------------------------------------------------------

function TSPBEPC.C_Color : TColor;
begin
  case ChartType of
    ctEPC: Result := EPC_C_Color;
    ctEIC: Result := EIC_C_Color;
    else Result := clBlack;
  end;
end;

//------------------------------------------------------------------------------

function TSPBEPC.D_Color : TColor;
begin
  case ChartType of
    ctEPC: Result := EPC_D_Color;
    ctEIC: Result := EIC_D_Color;
    else Result := clBlack;
  end;
end;

//------------------------------------------------------------------------------

function TSPBEPC.E_Color : TColor;
begin
  case ChartType of
    ctEPC: Result := EPC_E_Color;
    ctEIC: Result := EIC_E_Color;
    else Result := clBlack;
  end;
end;

//------------------------------------------------------------------------------

function TSPBEPC.F_Color : TColor;
begin
  case ChartType of
    ctEPC: Result := EPC_F_Color;
    ctEIC: Result := EIC_F_Color;
    else Result := clBlack;
  end;
end;

//------------------------------------------------------------------------------

function TSPBEPC.G_Color : TColor;
begin
  case ChartType of
    ctEPC: Result := EPC_G_Color;
    ctEIC: Result := EIC_G_Color;
    else Result := clBlack;
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := Msg.Result or DLGC_WANTARROWS;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.WMKeyDown(var Message: TWMKeyDown);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.WMKillFocus(var Message: TWMSetFocus);
begin
  inherited;
  Refresh;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if CurrentVisible and not ((SelectedCol = 3) and PotentialVisible) then
    SelectedCol := 2
  else if PotentialVisible then
    SelectedCol := 3;
  Refresh;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.KeyDown(var Key: Word; Shift: TShiftState);
  function GetNextRange(Value: Integer; GoForward: Boolean): Integer;
  begin
    Result := Value;
    case Value of
      0..20  : begin
                 if GoForward then
                   Result := 21
                 else
                   Result := 0;
               end;
      21..38 : begin
                 if GoForward then
                   Result := 39
                 else
                   Result := 0;
               end;
      39..54 : begin
                 if GoForward then
                   Result := 55
                 else
                   Result := 21;
               end;
      55..68 : begin
                 if GoForward then
                   Result := 69
                 else
                   Result := 39;
               end;
      69..80 : begin
                 if GoForward then
                   Result := 81
                 else
                   Result := 55;
               end;
      81..91 : begin
                 if GoForward then
                   Result := 92
                 else
                   Result := 69;
               end;
      92..100: begin
                 if GoForward then
                   Result := 100
                 else
                   Result := 81;
               end;
    end;
  end;
begin
  inherited;
  case key of
    VK_LEFT:
    begin
      if (SelectedCol > 2) and CurrentVisible then
        SelectedCol := 2;
    end;
    VK_RIGHT:
    begin
      if (SelectedCol < 3) and PotentialVisible then
        SelectedCol := 3;
    end;
    VK_UP:
    begin
      if (SelectedCol = 2) and CurrentVisible and not ReadOnly then
      begin
        Current := Min(100, Current + 1);
      end;

      if (SelectedCol = 3) and PotentialVisible and not ReadOnly then
      begin
        Potential := Min(100, Potential + 1);
      end;
    end;
    VK_DOWN:
    begin
      if (SelectedCol = 2) and CurrentVisible and not ReadOnly then
      begin
        Current := Max(0, Current - 1);
      end;

      if (SelectedCol = 3) and PotentialVisible and not ReadOnly then
      begin
        Potential := Max(0, Potential - 1);
      end;
    end;
    VK_PRIOR:
    begin
      if (SelectedCol = 2) and CurrentVisible and not ReadOnly then
      begin
        Current := Min(100, GetNextRange(Current, True));
      end;

      if (SelectedCol = 3) and PotentialVisible and not ReadOnly then
      begin
        Potential := Min(100, GetNextRange(Potential, True));
      end;
    end;
    VK_NEXT:
    begin
      if (SelectedCol = 2) and CurrentVisible and not ReadOnly then
      begin
        Current := Max(0, GetNextRange(Current, False));
      end;

      if (SelectedCol = 3) and PotentialVisible and not ReadOnly then
      begin
        Potential := Max(0, GetNextRange(Potential, False));
      end;
    end;
    VK_HOME:
    begin
      if (SelectedCol = 2) and CurrentVisible and not ReadOnly then
      begin
        Current := 0;
      end;

      if (SelectedCol = 3) and PotentialVisible and not ReadOnly then
      begin
        Potential := 0;
      end;
    end;
    VK_END:
    begin
      if (SelectedCol = 2) and CurrentVisible and not ReadOnly then
      begin
        Current := 100;
      end;

      if (SelectedCol = 3) and PotentialVisible and not ReadOnly then
      begin
        Potential := 100;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TSPBEPC.Loaded;
begin
  inherited;
  RebuildBaseImage := True;
  Refresh;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.BeginUpdate;
begin
  DoingUpdate := True;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.CMFontChanged(var Message: TMessage);
begin
  inherited;

  RebuildBaseImage := True;
  Refresh;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FMouseDraging := False;

  if Assigned(FOnMouseEnter) then
     FOnMouseEnter(Self);
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseDraging := False;

  if Assigned(FOnMouseLeave) then
     FOnMouseLeave(Self);
end;

//------------------------------------------------------------------------------

constructor TSPBEPC.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csSetCaption];
  BaseLocationDetails := TSPBEPCLocationDetails.Create;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;

  FCurrentTitleAutoSize := false;
  FPotentialTitleAutoSize := false;
  FTitleColor :=  $00BC7400;

  FTitleFont := TFont.Create;
  FTitleFont.Name := 'Tahoma';
  FTitleFont.Color := clWhite;
  FTitleFont.OnChange := OnFontChanged;

  FTopCaptionFont := TFont.Create;
  FTopCaptionFont.Name := 'Tahoma';
  FTopCaptionFont.Style := [fsItalic];
  FTopCaptionFont.OnChange := OnFontChanged;

  FBottomCaptionFont := TFont.Create;
  FBottomCaptionFont.Name := 'Tahoma';
  FBottomCaptionFont.Style := [fsItalic];
  FBottomCaptionFont.OnChange := OnFontChanged;

  RebuildBaseImage := True;
  BaseImage := TImage.Create(nil);
  FPenWidth := 1;
  FPenColor := clBlack;
//  FTitle := 'Energy Efficiency Rating';
//  FCurrentTitle := 'Current';
//  FPotentialTitle := 'Potential';
//  FTopCaption := 'Very energy efficient - lower running costs';
//  FBottomCaption := 'Not energy efficient - higher running costs';
  FChartType := ctEPC;
  FCurrentVisible := True;
  FPotentialVisible := True;
  FReadOnly := False;
  Refresh;
  Self.OnResize := Self.DoOnResize;
  FMouseDraging := False;
  FSelectedCol := -1;
  DoubleBuffered := True;
  Width := 320;
  Height := 250;
  FRepeatTimer := nil;
  Font.Name := 'Tahoma';
  FSlideDirUp := -1;
end;

//------------------------------------------------------------------------------

function TSPBEPC.DestRect: TRect;
var
  cw, ch: Integer;
begin
  cw := ClientWidth;
  ch := ClientHeight;

  with Result do begin
    Left := 0;
    Top := 0;
    Right := cw;
    Bottom := ch;
  end;
end;

//------------------------------------------------------------------------------

destructor TSPBEPC.Destroy;
begin
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;

  try
    FreeAndNil(BaseImage);
  except
    BaseImage := nil;
  end;
  try
    FreeAndNil(BaseLocationDetails);
  except
    BaseLocationDetails := nil;
  end;

  FPicture.Free;
  FTitleFont.Free;
  FTopCaptionFont.Free;
  FBottomCaptionFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.DoOnResize(Sender: TObject);
begin
  Self.Refresh;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.EndUpdate;
begin
  DoingUpdate := False;
  Refresh;
end;

//------------------------------------------------------------------------------

function TSPBEPC.GetBottomCaption: string;
begin
  if FBottomCaption = '' then
    case ChartType of
      ctEPC: Result := 'Not energy efficient - higher running costs';
      ctEIC: Result := 'Not environmentally friendly - higher CO2 emmissions';
    end
  else
    Result := FBottomCaption;
end;

//------------------------------------------------------------------------------

function TSPBEPC.GetColorForValue(Value: Integer): TColor;
begin
  case Value of
    0..20  : Result := G_Color;
    21..38 : Result := F_Color;
    39..54 : Result := E_Color;
    55..68 : Result := D_Color;
    69..80 : Result := C_Color;
    81..91 : Result := B_Color;
    92..100: Result := A_Color;
    else Result := clBlack;
  end;
end;

//------------------------------------------------------------------------------

function TSPBEPC.GetCurrentColor: TColor;
begin
  Result := GetColorForValue(Current);
end;

//------------------------------------------------------------------------------

function TSPBEPC.GetCurrentTitle: string;
begin
  if FCurrentTitle = '' then
    Result := 'Current'
  else
    Result := FCurrentTitle;
end;

//------------------------------------------------------------------------------

function TSPBEPC.GetPotentialColor: TColor;
begin
  Result := GetColorForValue(Potential);
end;

//------------------------------------------------------------------------------

function TSPBEPC.GetPotentialTitle: string;
begin
  if FPotentialTitle = '' then
    Result := 'Potential'
  else
    Result := FPotentialTitle;
end;

//------------------------------------------------------------------------------

function TSPBEPC.GetTitle: string;
begin
  if FTitle = '' then
    case ChartType of
      ctEPC: Result := 'Energy Efficiency Rating';
      ctEIC: Result := 'Environmental Impact CO2 Rating';
    end
  else
    Result := FTitle;
end;

//------------------------------------------------------------------------------

function TSPBEPC.GetTopCaption: string;
begin
  if FTopCaption = '' then
    case ChartType of
      ctEPC: Result := 'Very energy efficient - lower running costs';
      ctEIC: Result := 'Very environmentally friendly - lower CO2 emmissions';
    end
  else
    Result := FTopCaption;
end;

//------------------------------------------------------------------------------

function TSPBEPC.GetValueFromTopPosition(TopPosition: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  if (GetTopPosition(100) >= TopPosition) then begin
    Result := 100;
    Exit;
  end;

  for I := 0 to 100 do begin
    if (GetTopPosition(I) <= TopPosition) then begin
      Result := I;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TSPBEPC.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

function TSPBEPC.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

function TSPBEPC.GetTopPosition(Value: Integer): Integer;
var
  Lower, Higher : Integer;
  Ratio : Integer;
begin
  case Value of
    0..20  : begin
               Result := BaseLocationDetails.ATop + (BaseLocationDetails.StairSize * 6);
               Lower := 0;
               Higher := 20;
             end;
    21..38 : begin
               Result := BaseLocationDetails.ATop + (BaseLocationDetails.StairSize * 5);
               Lower := 21;
               Higher := 38;
             end;
    39..54 : begin
               Result := BaseLocationDetails.ATop + (BaseLocationDetails.StairSize * 4);
               Lower := 39;
               Higher := 54;
             end;
    55..68 : begin
               Result := BaseLocationDetails.ATop + (BaseLocationDetails.StairSize * 3);
               Lower := 55;
               Higher := 68;
             end;
    69..80 : begin
               Result := BaseLocationDetails.ATop + (BaseLocationDetails.StairSize * 2);
               Lower := 69;
               Higher := 80;
             end;
    81..91 : begin
               Result := BaseLocationDetails.ATop + (BaseLocationDetails.StairSize);
               Lower := 81;
               Higher := 91;
             end;
    92..100: begin
               Result := BaseLocationDetails.ATop;
               Lower := 92;
               Higher := 100;
             end;
    else     begin
               Result := 0;
               Exit;
             end;
  end;

  // mulitplied by the number of steps from the top of the range
  // And then half a step added so the color is in the middle of the step for the pointer
  // Size of the stair (color band) dividied by the number of numbers in the range,
  Result := Result - (BaseLocationDetails.StairSize div 2);
  Ratio := Trunc((BaseLocationDetails.StairSize - PenWidth) / (Higher-Lower) * (Higher - Value));
  Result := Result + Ratio;
end;

//------------------------------------------------------------------------------

function TSPBEPC.GetPointerRect(ColIndex: Integer): TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if (ColIndex = 2) then begin
    if CurrentVisible then begin
      Result.Top := GetTopPosition(Current);
      Result.Left := BaseLocationDetails.CurrentLeft+(PenWidth*2);
      Result.Right := BaseLocationDetails.CurrentLeft+BaseLocationDetails.CurrentWidth-(PenWidth*2);
      Result.Bottom := Result.Top+BaseLocationDetails.StairSize;
    end;
  end
  else if (ColIndex = 3) then begin
    if PotentialVisible then begin
      Result.Top := GetTopPosition(Potential);
      Result.Left := BaseLocationDetails.PotentialLeft+(PenWidth*2);
      Result.Right := BaseLocationDetails.PotentialLeft+BaseLocationDetails.PotentialWidth-(PenWidth*2);
      Result.Bottom := Result.Top+BaseLocationDetails.StairSize;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.Refresh;
var
  R, CurrentRect: TRect;
  PotentialRect: TRect;
  TenPercent: Integer;

  procedure DrawPointer(Rect : TRect; Color : TColor; Value : Integer);
  var
    Work : string;
  begin
    if not Assigned(BaseLocationDetails) then
      Exit;

    Self.Canvas.Pen.Style := psSolid;
    Self.Canvas.Pen.Color := clBlack;
    Self.Canvas.Brush.Style := bsSolid;
    Self.Canvas.Brush.Color := Color;

    // Border
    Self.Canvas.MoveTo(Rect.Left+(BaseLocationDetails.StairSize div 2),Rect.Top);
    Self.Canvas.LineTo(Rect.Right, Rect.Top);
    Self.Canvas.LineTo(Rect.Right, Rect.Bottom);
    Self.Canvas.LineTo(Rect.Left+(BaseLocationDetails.StairSize div 2),Rect.Bottom);
    Self.Canvas.LineTo(Rect.Left, Rect.Top+(BaseLocationDetails.StairSize div 2));
    Self.Canvas.LineTo(Rect.Left+(BaseLocationDetails.StairSize div 2),Rect.Top);

    Self.Canvas.FloodFill(Rect.Right-((Rect.Right-Rect.left) div 2),
                          Rect.Top+((Rect.Bottom-Rect.Top) div 2),
                          clBlack,fsBorder);

    // Over draw border
    Self.Canvas.Pen.Color := Color;
    Self.Canvas.MoveTo(Rect.Left+(BaseLocationDetails.StairSize div 2),Rect.Top);
    Self.Canvas.LineTo(Rect.Right, Rect.Top);
    Self.Canvas.LineTo(Rect.Right, Rect.Bottom);
    Self.Canvas.LineTo(Rect.Left+(BaseLocationDetails.StairSize div 2),Rect.Bottom);
    Self.Canvas.LineTo(Rect.Left, Rect.Top+(BaseLocationDetails.StairSize div 2));
    Self.Canvas.LineTo(Rect.Left+(BaseLocationDetails.StairSize div 2),Rect.Top);

    Work := IntToStr(Value);
    Rect.Left := Rect.Left+(BaseLocationDetails.StairSize div 2);
    MakeTextFit(Self.Canvas,Rect,Work);
    Self.Canvas.Font.Color := clWhite;
    Self.Canvas.Brush.Style := bsClear;
    //Self.Canvas.TextRect(Rect,Work,[tfLeft,tfVerticalCenter,tfSingleLine]);
    DrawTextRect(Self.Canvas, Rect,Work,[tfLeft,tfVerticalCenter,tfSingleLine]);
  end;

begin
  // Paint it!
  if DoingUpdate or not Assigned(FPicture) then
    Exit;

  if (RebuildBaseImage) or
    (Self.Width <> BaseImage.Width) or
    (Self.Height <> BaseImage.Height) then begin
    RefreshBaseImage;
  end;

{$IFDEF DELPHI2007_LVL}
  Self.Picture.Bitmap.SetSize(Self.Width,Self.Height);
{$ELSE}
  Self.Picture.Bitmap.Width := Self.Width;
  Self.Picture.Bitmap.Height := Self.Height;
{$ENDIF}

  Self.Canvas.Draw(0,0,BaseImage.Picture.Graphic);

  // Draw on the top pointers.
  CurrentRect.Top := GetTopPosition(Current);
  CurrentRect.Left := BaseLocationDetails.CurrentLeft+(PenWidth*2);
  CurrentRect.Right := BaseLocationDetails.CurrentLeft+BaseLocationDetails.CurrentWidth-(PenWidth*2);
  CurrentRect.Bottom := CurrentRect.Top+BaseLocationDetails.StairSize;

  if CurrentVisible then
    DrawPointer(CurrentRect,CurrentColor,Current);

  PotentialRect.Top := GetTopPosition(Potential);
  PotentialRect.Left := BaseLocationDetails.PotentialLeft+(PenWidth*2);
  PotentialRect.Right := BaseLocationDetails.PotentialLeft+BaseLocationDetails.PotentialWidth-(PenWidth*2);
  PotentialRect.Bottom := PotentialRect.Top+BaseLocationDetails.StairSize;

  if PotentialVisible then  
    DrawPointer(PotentialRect,PotentialColor,Potential);

  if (SelectedCol >= 1) and Focused then
  begin
    TenPercent := BaseImage.Height div 10;
    Self.Canvas.Pen.Color := clGreen;
    Self.Canvas.Brush.Color := clGreen;
    if (SelectedCol = 3) and PotentialVisible then begin
      R.Left := BaseLocationDetails.Potentialleft - (PenWidth div 2);
      R.Right := R.Left + BaseLocationDetails.PotentialWidth + PenWidth;
      R.Top := (TenPercent*2)+PenWidth - (PenWidth div 2);
      R.Bottom := BaseImage.Height-PenWidth - (PenWidth div 2);
      Self.Canvas.DrawFocusRect(R);
    end
    else if (SelectedCol = 2) and CurrentVisible then begin
      R.Left := BaseLocationDetails.CurrentLeft - (PenWidth div 2);
      R.Right := R.Left + BaseLocationDetails.CurrentWidth + PenWidth;
      if PotentialVisible then
        R.Right := R.Right + PenWidth;
      R.Top := (TenPercent*2)+PenWidth - (PenWidth div 2);
      R.Bottom := BaseImage.Height-PenWidth - (PenWidth div 2);
      Self.Canvas.DrawFocusRect(R);
    end;
  end;

  if HandleAllocated then
    Invalidate;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.MakeTextFit(Canvas : TCanvas; var Rect : TRect; Str : string);
begin
  Canvas.Font.Height := Rect.Bottom-Rect.Top;
  if Rect.Right-Rect.Left > 0 then
    while (Canvas.TextWidth(Str) >= Rect.Right-Rect.Left) do begin
      Canvas.Font.Height := Canvas.Font.Height -1;
      if Canvas.Font.Height <= 1 then
        Break;
    end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.ClickSlider;
var
  R: TRect;
begin
  if FMouseSliding and (SelectedCol >= 2) and not (csDesigning in ComponentState) and not ReadOnly then
  begin
    R := GetPointerRect(SelectedCol);
    if (R.Top < 0) then
      Exit;

    if (FSliderDownPt.Y <= (R.Top - 1 + BaseLocationDetails.StairSize div 2)) then begin
      if (FSlideDirUp = 0) then begin
        if Abs(FSliderDownPt.Y - (R.Top + BaseLocationDetails.StairSize div 2)) <= (BaseLocationDetails.StairSize div 2) then begin
          Exit;
        end;
      end;

      if (SelectedCol = 2) then
        Current := Min(100, Current + 1)
      else
        Potential := Min(100, Potential + 1);
      FSlideDirUp := 1;
    end
    else if (FSliderDownPt.Y > (R.Bottom + 1 - BaseLocationDetails.StairSize div 2)) then begin
      if (FSlideDirUp = 1) then begin
        if Abs(FSliderDownPt.Y - (R.Top + BaseLocationDetails.StairSize div 2)) <= (BaseLocationDetails.StairSize div 2) then begin
          Exit;
        end;
      end;

      if (SelectedCol = 2) then
        Current := Max(0, Current - 1)
      else
        Potential := Max(0, Potential - 1);
      FSlideDirUp := 0;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.RepeatTimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := 100;
  if MouseCapture then
  begin
    try
      if FMouseSliding then
        ClickSlider
      else
        FRepeatTimer.Enabled := False;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  R: TRect;
begin
  inherited;
  if (Button = mbLeft) and not (csDesigning in ComponentState) and not ReadOnly then
  begin
    SelectedCol := GetColAtPos(Point(X, Y));
    if (SelectedCol >= 2) and CanFocus and TabStop and not Focused then
      SetFocus;
    if (SelectedCol >= 2) then begin
      R := GetPointerRect(SelectedCol);
      R.Left := R.Left + (BaseLocationDetails.StairSize div 3);
      FMouseDraging := PtInRect(R, Point(X, Y));
      if FMouseDraging then begin     // Draging
      end
      else begin  // click outside Pointer, sliding
        FMouseSliding := True;
        FSlideDirUp := -1;
        FSliderDownPt := Point(X,Y);
        
        if FRepeatTimer = nil then
          FRepeatTimer := TTimer.Create(Self);

        FRepeatTimer.OnTimer := RepeatTimerExpired;
        FRepeatTimer.Interval := 400;
        FRepeatTimer.Enabled  := True;
      end
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  inherited;
  if not ReadOnly and FMouseSliding and (ssLeft in Shift) then begin
    FSliderDownPt := Point(X,Y);
  end;

  if not ReadOnly and FMouseDraging then begin
    i := GetValueFromTopPosition(Y - BaseLocationDetails.StairSize div 2);
    if (i >= 0) then begin
      if (SelectedCol = 2) and CurrentVisible then begin
        Current := Max(0, i);
      end
      else if (SelectedCol = 3) and PotentialVisible then begin
        Potential := Max(0, i);
      end;
    end;     
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  R: TRect;
  I: Integer;
begin
  inherited;
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled  := False;

  if (Button = mbLeft) and not (csDesigning in ComponentState) and not ReadOnly then
  begin
    I := GetColAtPos(Point(X, Y));

    if (SelectedCol >= 2) and (SelectedCol = I) then begin
      R := GetPointerRect(SelectedCol);
      R.Left := R.Left + (BaseLocationDetails.StairSize div 3);

      if not PtInRect(R, Point(X, Y)) then begin // click outside Pointer
        FSlideDirUp := -1;
        FSliderDownPt := Point(X,Y);
        ClickSlider;
        FSlideDirUp := -1;
      end
    end;
  end;

  FMouseSliding := False;
  FMouseDraging := False;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.Paint;
{$IFDEF DELPHI2007_LVL}
  procedure DoBufferedPaint(Canvas: TCanvas);
  var
    MemDC: HDC;
    Rect: TRect;
    PaintBuffer: HPAINTBUFFER;
  begin
    Rect := DestRect;
    PaintBuffer := BeginBufferedPaint(Canvas.Handle, Rect, BPBF_TOPDOWNDIB, nil, MemDC);
    try
      Canvas.Handle := MemDC;
      Canvas.StretchDraw(DestRect, Picture.Graphic);
      BufferedPaintMakeOpaque(PaintBuffer, @Rect);
    finally
      EndBufferedPaint(PaintBuffer, True);
    end;
  end;
{$ENDIF}

{$IFDEF DELPHI2007_LVL}
var
  PaintOnGlass: Boolean;
{$ENDIF}
begin
  if csDesigning in ComponentState then
    with inherited Canvas do
    begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;
  try
{$IFDEF DELPHI2007_LVL}
    PaintOnGlass := False;
    if PaintOnGlass then
      DoBufferedPaint(inherited Canvas)
    else
{$ENDIF}
      with inherited Canvas do
        StretchDraw(DestRect, Picture.Graphic);
  finally
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.PictureChanged(Sender: TObject);
begin
end;

//------------------------------------------------------------------------------

function TSPBEPC.GetCanvas: TCanvas;
var
  Bitmap: Graphics.TBitmap;
begin
  if not Assigned(FPicture) then
  begin
    Result := nil;
    Exit;
  end;

  if (FPicture.Graphic = nil) then
  begin
    Bitmap := Graphics.TBitmap.Create;
    try
      Bitmap.Width := Width;
      Bitmap.Height := Height;
      FPicture.Graphic := Bitmap;
    finally
      Bitmap.Free;
    end;
  end;
  if FPicture.Graphic is Graphics.TBitmap then
    Result := Graphics.TBitmap(FPicture.Graphic).Canvas
  else
    raise EInvalidOperation.Create(SImageCanvasNeedsBitmap);
end;

//------------------------------------------------------------------------------

function TSPBEPC.GetColAtPos(P: TPoint): Integer;
var
  R, R1: TRect;
  TenPercent: Integer;
begin
  Result := -1;
  if not Assigned(BaseImage) then
    Exit;
  
  TenPercent := BaseImage.Height div 10;
  R1.Left := 0;
  R1.Right := BaseImage.Width;
  R1.Top := TenPercent;
  R1.Bottom := BaseImage.Height-PenWidth;
  if PotentialVisible then begin
    R.Left := BaseLocationDetails.Potentialleft;
    R.Right := R.Left + BaseLocationDetails.PotentialWidth;
    R.Top := TenPercent;
    R.Bottom := BaseImage.Height-PenWidth;
    if PtInRect(R, p) then begin
      Result := 3;
      Exit;
    end;
    R1.Right := R.Left;
  end;

  if CurrentVisible then begin
    R.Left := BaseLocationDetails.CurrentLeft;
    R.Right := R.Left + BaseLocationDetails.CurrentWidth;
    R.Top := TenPercent;
    R.Bottom := BaseImage.Height-PenWidth;
    if PtInRect(R, p) then begin
      Result := 2;
      Exit;
    end;
    R1.Right := R.Left;
  end;

  if PtInRect(R1, p) then begin
    Result := 1;
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.RefreshBaseImage;
var
  Rect : TRect;
  TenPercent : Integer;
  FifteenPercent : Integer;
  HeaderBuffer : Integer;
  I: Integer;
  StepSize: Integer;
  TopRect, BottomRect, RangeRect: TRect;
  StairSize: Integer;
  Letter, Range, Work: string;
  MaxRangeHeight, X: Integer;
begin
  // Clear Background
//  BaseImage.Free;
//  BaseImage := TImage.Create(Owner);
{$IFDEF DELPHI2007_LVL}
  BaseImage.Picture.Bitmap.SetSize(Self.Width,Self.Height);
{$ELSE}
  BaseImage.Picture.Bitmap.Width := Self.Width;
  BaseImage.Picture.Bitmap.Height := Self.Height;
{$ENDIF}
  BaseImage.Width := Self.Width;
  BaseImage.Height := Self.Height;
  BaseImage.Canvas.Brush.Color := clWhite;
  Rect.Left := 0;
  Rect.Top := 0;
  Rect.Right := BaseImage.Width;
  Rect.Bottom := BaseImage.Height;
  BaseImage.Canvas.FillRect(Rect);

  TenPercent := BaseImage.Height div 10;
  FifteenPercent := Trunc(BaseImage.Width * 0.15);

  // Set Brush
  BaseImage.Canvas.MoveTo(0,0);
  BaseImage.Canvas.Brush.Style := bsSolid;
  // Top - Blue
  BaseImage.Canvas.Brush.Color := TitleColor;
  Rect.Left := 0;
  Rect.Top := 0;
  Rect.Right := BaseImage.Width;
  Rect.Bottom := TenPercent - PenWidth;
  BaseImage.Canvas.FillRect(Rect);
  // Top - Caption
  HeaderBuffer := PenWidth;
  Rect.Left := HeaderBuffer;
  Rect.Top := HeaderBuffer;
  Rect.Bottom := TenPercent - (HeaderBuffer+PenWidth);
  Rect.Right := BaseImage.Width - (HeaderBuffer*2);
  BaseImage.Canvas.Font.Assign(TitleFont);
  //BaseImage.Canvas.Font.Style := [];
  //BaseImage.Canvas.Font.Color := clWhite;
  //BaseImage.Canvas.Font.Name := Self.Font.Name;
  Work := Title;
  MakeTextFit(BaseImage.Canvas,Rect,Work);
  BaseImage.Canvas.Brush.Style := bsClear;
  //BaseImage.Canvas.TextRect(Rect,Work,[tfLeft,tfVerticalCenter,tfSingleLine]);
  DrawTextRect(BaseImage.Canvas, Rect,Work,[tfLeft,tfVerticalCenter,tfSingleLine]);

  // Column Headers
  Rect.Left := (BaseImage.Width-FifteenPercent)+PenWidth;
  Rect.Right := BaseImage.Width-(PenWidth*2);
  Rect.Top := TenPercent;
  Rect.Bottom := TenPercent*2;
  BaseImage.Canvas.Font.Color := clBlack;
  BaseImage.Canvas.Font.Name := Self.Font.Name;

  // Global Positioning....(left and right) - Must do before the text fitting is done!
  BaseLocationDetails.Potentialleft := Rect.Left;
  BaseLocationDetails.PotentialWidth := Rect.Right-Rect.Left;

  X := 0;
  BaseImage.Canvas.Brush.Color := clWhite;
  // Potential Column Caption Size
  if PotentialVisible then begin
    Work := PotentialTitle;
    BaseImage.Canvas.Font.Assign(Self.Font);
    if PotentialTitleAutoSize then    
      MakeTextFit(BaseImage.Canvas,Rect,Work);
    BaseImage.Canvas.Brush.Style := bsClear;
    //BaseImage.Canvas.TextRect(Rect,Work,[tfCenter,tfVerticalCenter,tfSingleLine]);
    DrawTextRect(BaseImage.Canvas, Rect,Work,[tfCenter,tfVerticalCenter,tfSingleLine]);
    X := FifteenPercent;
  end;

  // Current Column Caption Size
  Rect.Left := (BaseImage.Width - (FifteenPercent + X))+PenWidth;
  Rect.Right := (BaseImage.Width - (PenWidth*2)) - X;

  // Global Positioning....(left and right) - Must do before the text fitting is done!
  BaseLocationDetails.Currentleft := Rect.Left;
  BaseLocationDetails.CurrentWidth := Rect.Right-Rect.Left;

  if CurrentVisible then begin
    Work := CurrentTitle;
    BaseImage.Canvas.Font.Assign(Self.Font);
    if CurrentTitleAutoSize then    
      MakeTextFit(BaseImage.Canvas,Rect,Work);
    BaseImage.Canvas.Brush.Style := bsClear;
    //BaseImage.Canvas.TextRect(Rect,Work,[tfCenter,tfVerticalCenter,tfSingleLine]);
    DrawTextRect(BaseImage.Canvas, Rect,Work,[tfCenter,tfVerticalCenter,tfSingleLine]);
    X := FifteenPercent + X;
  end;

  //Top Caption
  TopRect.Top := (TenPercent*2)+PenWidth;
  TopRect.Left := PenWidth*2;
  TopRect.Right := BaseImage.Width-(TopRect.Left*2)-(X);
  TopRect.Bottom := TopRect.Top +(TenPercent div 2);
  Work := TopCaption;
  BaseImage.Canvas.Font.Assign(TopCaptionFont);
  MakeTextFit(BaseImage.Canvas,TopRect,TopCaption);
  BaseImage.Canvas.Brush.Style := bsClear;
  //BaseImage.Canvas.Font.Style := [fsItalic];
  //BaseImage.Canvas.TextRect(TopRect,Work,[tfleft,tfVerticalCenter,tfSingleLine]);
  DrawTextRect(BaseImage.Canvas, TopRect,Work,[tfleft,tfVerticalCenter,tfSingleLine]);

  //Bottom Caption
  BottomRect.Top := BaseImage.Height - (PenWidth*2) - (TenPercent div 2);
  BottomRect.Bottom := BottomRect.Top +(TenPercent div 2);
  BottomRect.Left := TopRect.Left;
  BottomRect.Right := TopRect.Right;
  Work := BottomCaption;
  BaseImage.Canvas.Font.Assign(BottomCaptionFont);
  MakeTextFit(BaseImage.Canvas,BottomRect,Work);
  BaseImage.Canvas.Brush.Style := bsClear;
  //BaseImage.Canvas.Font.Style := [fsItalic];
  //BaseImage.Canvas.TextRect(BottomRect,Work,[tfleft,tfVerticalCenter,tfSingleLine]);
  DrawTextRect(BaseImage.Canvas, BottomRect,Work,[tfleft,tfVerticalCenter,tfSingleLine]);

  // Now the chart
  StepSize := Trunc((BaseImage.Width-(X)) * 0.105);
  StairSize := (BottomRect.Top - TopRect.Bottom) div 7;

  Rect.Top := TopRect.Bottom+PenWidth;
  Rect.Left := PenWidth;
  Rect.Right := Trunc((BaseImage.Width-(X)) * 0.34);
  Rect.Bottom := Rect.Top+StairSize-PenWidth;

  BaseLocationDetails.StairSize := StairSize;
  BaseLocationDetails.ATop := Rect.Top;

  RangeRect := Rect;
  RangeRect.Left := Rect.Left + PenWidth;
  RangeRect.Right := Trunc(Rect.Right * 0.66);
  MaxRangeHeight := 0;

  BaseImage.Canvas.Font.Assign(Self.Font);
  for I := 1 to 7 do begin
    if I <> 1 then begin
      Rect.Bottom := Rect.Bottom+StairSize; // Must do Bottom first.
      Rect.Top := Rect.Top+StairSize; // Then Top.
      Rect.Right := Rect.Right+StepSize;
    end;
    BaseImage.Canvas.Font.Color := clWhite;
    BaseImage.Canvas.Font.Style := [];
    case I of
      1 : begin
            BaseImage.Canvas.Brush.Color := A_Color;
            Letter := 'A';
          end;
      2 : begin
            BaseImage.Canvas.Brush.Color := B_Color;
            Letter := 'B';
          end;
      3 : begin
            BaseImage.Canvas.Brush.Color := C_Color;
            Letter := 'C';
          end;
      4 : begin
            BaseImage.Canvas.Brush.Color := D_Color;
            Letter := 'D';
          end;
      5 : begin
            BaseImage.Canvas.Brush.Color := E_Color;
            Letter := 'E';
          end;
      6 : begin
            BaseImage.Canvas.Brush.Color := F_Color;
            Letter := 'F';
          end;
      7 : begin
            BaseImage.Canvas.Brush.Color := G_Color;
            Letter := 'G';
          end;
    end;
    BaseImage.Canvas.FillRect(Rect);
    MakeTextFit(BaseImage.Canvas,Rect,Letter);
    Rect.Right := Rect.Right - 3;
    BaseImage.Canvas.Brush.Style := bsClear;
    //BaseImage.Canvas.TextRect(Rect,Letter,[tfRight,tfVerticalCenter,tfSingleLine]);
    DrawTextRect(BaseImage.Canvas, Rect,Letter,[tfRight,tfVerticalCenter,tfSingleLine]);
    Rect.Right := Rect.Right + 3;

    RangeRect.Top := Rect.Top;
    RangeRect.Bottom := Rect.Bottom;

    case I of
      1 : Range := '(92-100)';
      2 : Range := '(81-91)';
      3 : Range := '(69-80)';
      4 : Range := '(55-68)';
      5 : Range := '(39-54)';
      6 : Range := '(21-38)';
      7 : Range := '(1-20)';
    end;

    case ChartType of
      ctEPC: begin
               if I >= 3 then
                 BaseImage.Canvas.Font.Color := clBlack;
             end;
      ctEIC: begin
               if I <= 2 then
                 BaseImage.Canvas.Font.Color := clBlack
               else
                 BaseImage.Canvas.Font.Color := clWhite;
             end;
    end;



    MakeTextFit(BaseImage.Canvas,RangeRect, Range);

    if I = 1 then
      MaxRangeHeight := BaseImage.Canvas.Font.Height
    else if BaseImage.Canvas.Font.Height > MaxRangeHeight then
      BaseImage.Canvas.Font.Height := MaxRangeHeight;

    BaseImage.Canvas.Brush.Style := bsClear;
    //BaseImage.Canvas.TextRect(RangeRect,Range,[tfLeft,tfVerticalCenter,tfSingleLine]);
    DrawTextRect(BaseImage.Canvas, RangeRect,Range,[tfLeft,tfVerticalCenter,tfSingleLine]);
  end;
  // Build Border
  BaseImage.Canvas.Font.Style := Self.Font.Style;
  BaseImage.Canvas.Pen.Style := psSolid;
  BaseImage.Canvas.MoveTo(PenWidth,TenPercent);
  BaseImage.Canvas.Pen.Color := PenColor; //clBlack;
  BaseImage.Canvas.Pen.Width := PenWidth;
  BaseImage.Canvas.LineTo(BaseImage.Width-BaseImage.Canvas.Pen.Width,TenPercent);
  BaseImage.Canvas.LineTo(BaseImage.Width-BaseImage.Canvas.Pen.Width,BaseImage.Height-PenWidth);
  BaseImage.Canvas.LineTo(PenWidth,BaseImage.Height-PenWidth);
  BaseImage.Canvas.LineTo(PenWidth,TenPercent);
  // Top Row
  BaseImage.Canvas.MoveTo(PenWidth,TenPercent*2);
  BaseImage.Canvas.LineTo(BaseImage.Width-BaseImage.Canvas.Pen.Width,TenPercent*2);
  
  // Columns
  X := 0;
  if PotentialVisible then begin
    BaseImage.Canvas.MoveTo(BaseImage.Width-FifteenPercent,TenPercent);
    BaseImage.Canvas.LineTo(BaseImage.Width-FifteenPercent,BaseImage.Height-PenWidth);
    X := FifteenPercent;
  end;
  
  if CurrentVisible then begin
    BaseImage.Canvas.MoveTo(BaseImage.Width-(FifteenPercent + X),TenPercent);
    BaseImage.Canvas.LineTo(BaseImage.Width-(FifteenPercent + X),BaseImage.Height-PenWidth);
  end;

  RebuildBaseImage := False;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.SaveToFile(FileName: string);
begin
  if Assigned(FPicture) then  
    FPicture.SaveToFile(FileName);
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.SetBottomCaption(const Value: string);
begin
  if BottomCaption <> Value then begin
    FBottomCaption := Value;
    RebuildBaseImage := True;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.SetBottomCaptionFont(const Value: TFont);
begin
  FBottomCaptionFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.SetChartType(const Value: TSPBEPCChartType);
begin
  if FChartType <> Value then begin
    FChartType := Value;
    RebuildBaseImage := True;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.SetCurrent(const Value: Integer);
begin
  if (Value < 0) or (Value > 100) then
    Exit;
  if Current = Value then
    Exit;

  FCurrent := Value;
  if Assigned(FOnCurrentChange) and not (csDesigning in ComponentState) then
    FOnCurrentChange(Self);
  Refresh;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.SetCurrentTitle(const Value: string);
begin
  if CurrentTitle <> Value then begin
    FCurrentTitle := Value;
    RebuildBaseImage := True;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.SetCurrentTitleAutoSize(const Value: Boolean);
begin
  if (FCurrentTitleAutoSize <> Value) then begin
    FCurrentTitleAutoSize := Value;
    RebuildBaseImage := True;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.SetCurrentVisible(const Value: Boolean);
begin
  if (FCurrentVisible <> Value) then begin
    FCurrentVisible := Value;
    RefreshBaseImage;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.SetPenColor(const Value: TColor);
begin
  if (FPenColor <> Value) then begin
    FPenColor := Value;
    RebuildBaseImage := True;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.SetPenWidth(const Value: Integer);
begin
  if FPenWidth <> Value then begin
    FPenWidth := Value;
    RebuildBaseImage := True;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.SetPotential(const Value: Integer);
begin
  if (Value < 0) or (Value > 100) then
    Exit;
  if Potential = Value then
    Exit;

  FPotential := Value;
  if Assigned(FOnPotentialChange) and not (csDesigning in ComponentState) then
    FOnPotentialChange(Self);
  Refresh;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.SetPotentialTitle(const Value: string);
begin
  if PotentialTitle <> Value then begin
    FPotentialTitle := Value;
    RebuildBaseImage := True;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.SetPotentialTitleAutoSize(const Value: Boolean);
begin
  if (FPotentialTitleAutoSize <> Value) then begin
    FPotentialTitleAutoSize := Value;
    RebuildBaseImage := True;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.SetPotentialVisible(const Value: Boolean);
begin
  if (FPotentialVisible <> Value) then begin  
    FPotentialVisible := Value;
    RefreshBaseImage;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.SetSelectedCol(const Value: Integer);
begin
  if (FSelectedCol <> Value) then begin
    FSelectedCol := Value;
    RebuildBaseImage := True;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.SetTitle(const Value: string);
begin
  if Title <> Value then begin
    FTitle := Value;
    RebuildBaseImage := True;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.SetTitleColor(const Value: TColor);
begin
  if (FTitleColor <> Value) then begin
    FTitleColor := Value;
    RebuildBaseImage := True;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.OnFontChanged(Sender: TObject);
begin
  RebuildBaseImage := True;
  Refresh;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.SetTitleFont(const Value: TFont);
begin
  FTitleFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.SetTopCaption(const Value: string);
begin
  if TopCaption <> Value then begin
    FTopCaption := Value;
    RebuildBaseImage := True;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TSPBEPC.SetTopCaptionFont(const Value: TFont);
begin
  FTopCaptionFont.Assign(Value);
end;

//------------------------------------------------------------------------------


{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}


end.
