
{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2015                                        }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit AdvTreeView;

interface

{$I TMSDEFS.INC}

uses
  Windows, Classes, AdvCustomTreeView, AdvTreeViewData, AdvTreeViewBase, Graphics,
  Types, Messages, ImgList, PictureContainer
  {$IFDEF DELPHIXE2_LVL}
  , UITypes
  {$ENDIF}
  ,Controls, Forms
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 2; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.0.1 : Fixed : Issue with missing PictureContainer for HTML text
  // v1.0.0.2 : Improved : Issue with missing picture cache for loading local or URL images

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvTreeView = class(TAdvTreeViewPublished)
  private
    FIsWinXP: Boolean;
    FPictureCache: THTMLPictureCache;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
  protected
    function GetVersion: string; override;
    function CalculateText(ACanvas: TCanvas; AText: String; AWordWrapping: Boolean; ARect: TRectF): TRectF; override;
    function DrawText(ACanvas: TCanvas; ARect: TRectF; AHorizontalAlign, AVerticalAlign: TAdvTreeViewTextAlign; AText: String; ATrimming: TAdvTreeViewTextTrimming = tvttNone; AAngle: Single = 0;
      AReverseAlignment: Boolean = True; ASupportHTML: Boolean = False; ATestAnchor: Boolean = False; AWordWrapping: Boolean = False; AX: Single = -1; AY: Single = - 1; AMinWidth: Single = -1; AMinHeight: Single = -1): String; override;

    procedure DrawBorders; override;
    procedure FixStroke(ACanvas: TCanvas); virtual;
    procedure DrawEmptySpaces; override;
    procedure DrawNodeColumns; override;
    procedure DrawNode(ACanvas: TCanvas; ARect: TRectF; ANode: TAdvTreeViewVirtualNode; ACaching: Boolean = False); override;
    procedure DrawGroup(ACanvas: TCanvas; ARect: TRectF; AGroup: Integer; AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind); override;
    procedure DrawColumn(ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PictureCache: THTMLPictureCache read FPictureCache;
  end;

implementation

uses
  Math, SysUtils, ShellApi, CommCtrl, UxTheme, AdvXPVS;

{$I HTMLENGO.PAS}

procedure DrawGradient(Canvas: TCanvas; FromColor,ToColor: TColor; Steps: Integer;R:TRect; Direction: Boolean);
var
  diffr,startr,endr: Integer;
  diffg,startg,endg: Integer;
  diffb,startb,endb: Integer;
  iend: Integer;
  rstepr,rstepg,rstepb,rstepw: Real;
  i,stepw: Word;
  oldp, oldb: TColor;

begin
  if Steps = 0 then
    Steps := 1;

  if Steps > 32 then
  	Steps := 32;

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
    oldb := Brush.Color;
    oldp := Pen.Color;
    for i := 0 to Steps - 1 do
    begin
      endr := startr + Round(rstepr*i);
      endg := startg + Round(rstepg*i);
      endb := startb + Round(rstepb*i);
      stepw := Round(i*rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
      begin
        iend := R.Left + stepw + Trunc(rstepw) + 1;
        if iend > R.Right then
          iend := R.Right;
        Rectangle(R.Left + stepw,R.Top,iend,R.Bottom)
      end
      else
      begin
        iend := R.Top + stepw + Trunc(rstepw)+1;
        if iend > r.Bottom then
          iend := r.Bottom;
        Rectangle(R.Left,R.Top + stepw,R.Right,iend);
      end;
    end;
    Pen.Color := oldp;
    Brush.Color := oldb;
  end;
end;

{ TAdvTreeView }

function TAdvTreeView.CalculateText(ACanvas: TCanvas;
  AText: String; AWordWrapping: Boolean;
  ARect: TRectF): TRectF;
var
  dstyle: DWORD;
  r: TRect;
  a, s: String;
  fa: String;
  XSize, YSize: Integer;
  hl, ml: Integer;
  hr: TRect;
begin
  if Round(ARect.Width) <= 0 then
  begin
    ARect.Height := 0;
    Result := ARect;
    Exit;
  end;

  dstyle := DT_CALCRECT;
  if AWordWrapping then
    dstyle := dstyle or DT_WORDBREAK
  else
    dstyle := dstyle or DT_SINGLELINE;

  r := Rect(Round(ARect.Left), Round(ARect.Top), Round(ARect.Right), Round(ARect.Bottom));

  if ((AnsiPos('</', AText) > 0) or (AnsiPos('/>', AText)  > 0) or (AnsiPos('<BR>', UpperCase(AText)) > 0)) then
  begin
    HTMLDrawEx(ACanvas, AText, r, nil, 0, 0,-1,-1,0,False,True,False,False,False,False,AWordWrapping,1.0,clBlue,
      clNone,clNone,clNone,a,s,fa,XSize,YSize,hl,ml,hr, PictureCache, PictureContainer, 0);

    r.Right := r.Left + XSize;
    r.Bottom := r.Top + YSize;
  end
  else
    Windows.DrawText(ACanvas.Handle, PChar(AText), Length(AText), r, dstyle);

  Result := RectF(r.Left, r.Top, r.Right, r.Bottom);
end;

procedure TAdvTreeView.CMDialogKey(var Message: TCMDialogKey);
begin
  inherited;
  HandleDialogKey(Message.CharCode, KeyDataToShiftState(Message.KeyData));
end;

constructor TAdvTreeView.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  TabStop := True;
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;
  FIsWinXP := (i > 5);
  FPictureCache := THTMLPictureCache.Create;
end;

procedure TAdvTreeView.DblClick;
var
  pf: TPoint;
begin
  inherited;
  pf := ScreenToClient(Mouse.CursorPos);
  HandleDblClick(pf.X, pf.Y);
end;

destructor TAdvTreeView.Destroy;
begin
  FPictureCache.ClearPictures;
  FPictureCache.Free;
  inherited;
end;

procedure TAdvTreeView.DrawBorders;
var
  st: TAdvTreeViewCanvasSaveState;
  rrt, rrb, trl, grt, grb: TRectF;
  nr: TRectF;
begin
  inherited;
  st := SaveStateEx(Canvas);
  grt := GetGroupsTopRect;
  grb := GetGroupsBottomRect;
  rrt := GetColumnsTopRect;
  rrb := GetColumnsBottomRect;
  nr := GetContentClipRect;

  trl := RectF(Round(nr.Right) + 1.5, rrt.Bottom, Round(nr.Right) + 1.5, rrb.Top);


  Canvas.MoveTo(Round(trl.Right), Round(trl.Top));
  Canvas.Lineto(Round(trl.Right), Round(trl.Bottom));


  if HorizontalScrollBar.Visible or ColumnsAppearance.Stretch then
  begin
    Canvas.Pen.Assign(ColumnsAppearance.TopStroke);
    if (tclTop in ColumnsAppearance.Layouts) and (ColumnsAppearance.TopSize > 0) then
    begin
      Canvas.MoveTo(Round(rrt.Left), Round(rrt.Top));
      Canvas.Lineto(Round(rrt.Left), Round(rrt.Bottom));
      Canvas.MoveTo(Round(rrt.Right - 1), Round(rrt.Top));
      Canvas.Lineto(Round(rrt.Right - 1), Round(rrt.Bottom));
    end
    else if not (tglTop in GroupsAppearance.Layouts) or (GroupsAppearance.TopSize <= 0) or (DisplayGroups.Count = 0) then
    begin
      Canvas.MoveTo(Round(rrt.Left), Round(rrt.Bottom));
      Canvas.Lineto(Round(rrt.Right - 1), Round(rrt.Bottom));
    end;

    Canvas.Pen.Assign(ColumnsAppearance.BottomStroke);
    if (tclBottom in ColumnsAppearance.Layouts) and (ColumnsAppearance.Bottomsize > 0) then
    begin
      Canvas.MoveTo(Round(rrb.Left), Round(rrb.Top));
      Canvas.Lineto(Round(rrb.Left), Round(rrb.Bottom));
      Canvas.MoveTo(Round(rrb.Right - 1), Round(rrb.Top));
      Canvas.Lineto(Round(rrb.Right - 1), Round(rrb.Bottom));
    end
    else if not (tglBottom in GroupsAppearance.Layouts) or (GroupsAppearance.BottomSize <= 0) or (DisplayGroups.Count = 0) then
    begin
      Canvas.MoveTo(Round(rrb.Left), Round(rrb.Top));
      Canvas.Lineto(Round(rrb.Right - 1), Round(rrb.Top));
    end;

    Canvas.Pen.Assign(GroupsAppearance.TopStroke);
    if (tglTop in GroupsAppearance.Layouts) and (DisplayGroups.Count > 0) and (GroupsAppearance.TopSize > 0) then
    begin
      Canvas.MoveTo(Round(grt.Left), Round(grt.Top));
      Canvas.Lineto(Round(grt.Left), Round(grt.Bottom));
      Canvas.MoveTo(Round(grt.Right - 1), Round(grt.Top));
      Canvas.Lineto(Round(grt.Right - 1), Round(grt.Bottom));
    end;

    if not (tclTop in ColumnsAppearance.Layouts) or (ColumnsAppearance.TopSize <= 0) then
    begin
      Canvas.MoveTo(Round(grt.Left), Round(grt.Bottom));
      Canvas.Lineto(Round(grt.Right - 1), Round(grt.Bottom));
    end;

    Canvas.Pen.Assign(GroupsAppearance.BottomStroke);
    if (tglBottom in GroupsAppearance.Layouts) and (DisplayGroups.Count > 0) and (GroupsAppearance.BottomSize > 0) then
    begin
      Canvas.MoveTo(Round(grb.Left), Round(grb.Top));
      Canvas.Lineto(Round(grb.Left), Round(grb.Bottom));
      Canvas.MoveTo(Round(grb.Right - 1), Round(grb.Top));
      Canvas.Lineto(Round(grb.Right - 1), Round(grb.Bottom));
    end;

    if not (tclBottom in ColumnsAppearance.Layouts) or (ColumnsAppearance.BottomSize <= 0) then
    begin
      Canvas.MoveTo(Round(grb.Left - 1), Round(grb.Top));
      Canvas.Lineto(Round(grb.Right - 1), Round(grb.Top));
    end;
  end;

  RestoreStateEx(st, Canvas);
end;

procedure TAdvTreeView.DrawColumn(ACanvas: TCanvas; ARect: TRectF;
  AColumn: Integer; AKind: TAdvTreeViewCacheItemKind);
var
  st, sttxt: TAdvTreeViewCanvasSaveState;
  b: Boolean;
  str: String;
  df: Boolean;
  txtr: TRectF;
  def: Boolean;
  col: TAdvTreeViewColumn;
  br: TAdvTreeViewBrush;
  drawr: TRect;
  trim: TAdvTreeViewTextTrimming;
  ha, va: TAdvTreeViewTextAlign;
  ww: Boolean;
begin
  inherited;
  st := SaveStateEx(ACanvas);

  def := True;
  col := nil;
  br := nil;
  if (AColumn >= 0) and (AColumn <= Columns.Count - 1) then
  begin
    col := Columns[AColumn];
    if not col.UseDefaultAppearance then
    begin
      def := False;
      case AKind of
        ikColumnTop:
        begin
          ACanvas.Pen.Assign(col.TopStroke);
          br := col.TopFill;
          ACanvas.Brush.Assign(br);
        end;
        ikColumnBottom:
        begin
          ACanvas.Pen.Assign(col.BottomStroke);
          br := col.BottomFill;
          ACanvas.Brush.Assign(br);
        end;
      end;
    end;
  end;

  if def then
  begin
    case AKind of
      ikColumnTop:
      begin
        ACanvas.Pen.Assign(ColumnsAppearance.TopStroke);
        br := ColumnsAppearance.TopFill;
        ACanvas.Brush.Assign(br);
      end;
      ikColumnBottom:
      begin
        ACanvas.Pen.Assign(ColumnsAppearance.BottomStroke);
        br := ColumnsAppearance.BottomFill;
        ACanvas.Brush.Assign(br);
      end;
    end;
  end;

  FixStroke(ACanvas);

  b := True;
  df := True;
  DoBeforeDrawColumnHeader(ACanvas, ARect, AColumn, AKind, b, df);

  if b then
  begin
    if df then
    begin
      drawr := Rect(Floor(ARect.Left), Floor(ARect.Top), Round(ARect.Right + 0.5), Round(ARect.Bottom + 0.5));

      if Assigned(br) and (br.Kind = tvbkGradient) then
      begin
        DrawGradient(ACanvas, br.Gradient.Color, br.Gradient.ColorTo, 32, drawr, False);
        ACanvas.Brush.Style := bsClear;
      end;

      ACanvas.Rectangle(drawr);
    end;

    sttxt := SaveStateEx(ACanvas);
    if def then
    begin
      case AKind of
        ikColumnTop:
        begin
          ACanvas.Font.Assign(ColumnsAppearance.TopFont);
          ACanvas.Font.Color := ColumnsAppearance.TopFontColor;
        end;
        ikColumnBottom:
        begin
          ACanvas.Font.Assign(ColumnsAppearance.BottomFont);
          ACanvas.Font.Color := ColumnsAppearance.BottomFontColor;
        end;
      end;
    end
    else if Assigned(col) then
    begin
      case AKind of
        ikColumnTop:
        begin
          ACanvas.Font.Assign(col.TopFont);
          ACanvas.Font.Color := col.TopFontColor;
        end;
        ikColumnBottom:
        begin
          ACanvas.Font.Assign(col.BottomFont);
          ACanvas.Font.Color := col.BottomFontColor;
        end;
      end;
    end;

    str := GetColumnText(AColumn);
    DoGetColumnText(AColumn, AKind, str);

    ha := tvtaLeading;
    va := tvtaCenter;
    ww := False;
    trim := tvttNone;
    if Assigned(col) then
    begin
      ha := col.HorizontalTextAlign;
      va := col.VerticalTextAlign;
      ww := col.WordWrapping;
      trim := col.Trimming;
    end;

    DoGetColumnTrimming(AColumn, AKind, trim);
    DoGetColumnWordWrapping(AColumn, AKind, ww);
    DoGetColumnHorizontalTextAlign(AColumn, AKind, ha);
    DoGetColumnVerticalTextAlign(AColumn, AKind, va);

    b := True;
    txtr := ARect;
    InflateRectEx(txtr, -2, -2);
    DoBeforeDrawColumnText(ACanvas, txtr, AColumn, AKind, str, b);
    if b then
    begin
      case AKind of
        ikColumnTop:
        begin
          if ColumnsAppearance.TopVerticalText then
            DrawText(ACanvas, txtr, ha, va, str, trim, -90, False, True, False, ww)
          else
            DrawText(ACanvas, txtr, ha, va, str, trim, 0, False, True, False, ww);
        end;
        ikColumnBottom:
        begin
          if ColumnsAppearance.BottomVerticalText then
            DrawText(ACanvas, txtr, ha, va, str, trim, 90, False, True, False, ww)
          else
            DrawText(ACanvas, txtr, ha, va, str, trim, 0, False, True, False, ww);
        end;
      end;
      DoAfterDrawColumnText(ACanvas, txtr, AColumn, AKind, str);
    end;

    RestoreStateEx(sttxt, ACanvas);
    DoAfterDrawColumnHeader(ACanvas, ARect, AColumn, AKind);
  end;

  RestoreStateEx(st, ACanvas);
end;

procedure TAdvTreeView.DrawEmptySpaces;
var
  r: TRectF;
  st: TAdvTreeViewCanvasSaveState;
  b, df: Boolean;
  br: TAdvTreeViewBrush;
  drawr: TRect;
begin
  inherited;
  if ColumnsAppearance.FillEmptySpaces and not StretchScrollBars then
  begin
    if (tclTop in ColumnsAppearance.Layouts) and (ColumnsAppearance.TopSize > 0) then
    begin
      //Column top right
      r := GetColumnTopRightEmptyRect;
      r := RectF(Int(r.Left) - 0.5, Int(r.Top) + 0.5, Int(r.Right) + 0.5, Int(r.Bottom) + 0.5);
      st := SaveStateEx(Canvas);
      b := True;
      df := True;

      br := ColumnsAppearance.TopFill;
      Canvas.Brush.Assign(br);
      Canvas.Pen.Assign(ColumnsAppearance.TopStroke);

      DoBeforeDrawColumnEmptySpace(Canvas, r, tcesTopRight, b, df);
      if b then
      begin
        if df then
        begin
          drawr := Rect(Floor(r.Left), Floor(r.Top), Round(r.Right + 0.5), Round(r.Bottom + 0.5));

          if Assigned(br) and (br.Kind = tvbkGradient) then
          begin
            DrawGradient(Canvas, br.Gradient.Color, br.Gradient.ColorTo, 32, drawr, False);
            Canvas.Brush.Style := bsClear;
          end;

          Canvas.Rectangle(drawr);
        end;
        DoAfterDrawColumnEmptySpace(Canvas, r, tcesTopRight);
      end;
      RestoreStateEx(st, Canvas);
    end;

    if (tclBottom in ColumnsAppearance.Layouts) and (ColumnsAppearance.BottomSize > 0) then
    begin
      //Column bottom right
      r := GetColumnBottomRightEmptyRect;
      r := RectF(Int(r.Left) - 0.5, Int(r.Top) - 0.5, Int(r.Right) + 0.5, Int(r.Bottom) - 0.5);
      st := SaveStateEx(Canvas);
      b := True;
      df := True;

      br := ColumnsAppearance.BottomFill;
      Canvas.Brush.Assign(br);
      Canvas.Pen.Assign(ColumnsAppearance.BottomStroke);

      DoBeforeDrawColumnEmptySpace(Canvas, r, tcesBottomRight, b, df);
      if b then
      begin
        if df then
        begin
          drawr := Rect(Floor(r.Left), Floor(r.Top), Round(r.Right + 0.5), Round(r.Bottom + 0.5));

          if Assigned(br) and (br.Kind = tvbkGradient) then
          begin
            DrawGradient(Canvas, br.Gradient.Color, br.Gradient.ColorTo, 32, drawr, False);
            Canvas.Brush.Style := bsClear;
          end;

          Canvas.Rectangle(drawr);
        end;
        DoAfterDrawColumnEmptySpace(Canvas, r, tcesBottomRight);
      end;
      RestoreStateEx(st, Canvas);
    end;
  end;

  if GroupsAppearance.FillEmptySpaces and not StretchScrollBars then
  begin
    if (tglTop in GroupsAppearance.Layouts) and (GroupsAppearance.TopSize > 0) then
    begin
      //Group top right
      r := GetGroupTopRightEmptyRect;
      r := RectF(Int(r.Left) - 0.5, Int(r.Top) + 0.5, Int(r.Right) - 0.5, Int(r.Bottom) + 0.5);
      st := SaveStateEx(Canvas);
      b := True;
      df := True;

      br := GroupsAppearance.TopFill;
      Canvas.Brush.Assign(br);
      Canvas.Pen.Assign(GroupsAppearance.TopStroke);

      DoBeforeDrawGroupEmptySpace(Canvas, r, tgesTopRight, b, df);
      if b then
      begin
        if df then
        begin
          drawr := Rect(Floor(r.Left), Floor(r.Top), Round(r.Right + 0.5), Round(r.Bottom + 0.5));

          if Assigned(br) and (br.Kind = tvbkGradient) then
          begin
            DrawGradient(Canvas, br.Gradient.Color, br.Gradient.ColorTo, 32, drawr, False);
            Canvas.Brush.Style := bsClear;
          end;

          Canvas.Rectangle(drawr);
        end;
        DoAfterDrawGroupEmptySpace(Canvas, r, tgesTopRight);
      end;
      RestoreStateEx(st, Canvas);
    end;

    if (tglBottom in GroupsAppearance.Layouts) and (GroupsAppearance.BottomSize > 0) then
    begin
      //Group bottom right
      r := GetGroupBottomRightEmptyRect;
      r := RectF(Int(r.Left) - 0.5, Int(r.Top) - 0.5, Int(r.Right) - 0.5, Int(r.Bottom) - 0.5);
      st := SaveStateEx(Canvas);
      b := True;
      df := True;

      br := GroupsAppearance.BottomFill;
      Canvas.Brush.Assign(br);
      Canvas.Pen.Assign(GroupsAppearance.BottomStroke);

      DoBeforeDrawGroupEmptySpace(Canvas, r, tgesBottomRight, b, df);
      if b then
      begin
        if df then
        begin
          drawr := Rect(Floor(r.Left), Floor(r.Top), Round(r.Right + 0.5), Round(r.Bottom + 0.5));

          if Assigned(br) and (br.Kind = tvbkGradient) then
          begin
            DrawGradient(Canvas, br.Gradient.Color, br.Gradient.ColorTo, 32, drawr, False);
            Canvas.Brush.Style := bsClear;
          end;

          Canvas.Rectangle(drawr);
        end;
        DoAfterDrawGroupEmptySpace(Canvas, r, tgesBottomRight);
      end;
      RestoreStateEx(st, Canvas);
    end;
  end;
end;

procedure TAdvTreeView.DrawGroup(ACanvas: TCanvas; ARect: TRectF; AGroup,
  AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind);
var
  st, sttxt: TAdvTreeViewCanvasSaveState;
  b, df: Boolean;
  str: String;
  txtr: TRectF;
  def: Boolean;
  grp: TAdvTreeViewGroup;
  br: TAdvTreeViewBrush;
  drawr: TRect;
begin
  st := SaveStateEx(ACanvas);

  def := True;
  grp := nil;
  br := nil;
  if (AGroup >= 0) and (AGroup <= Groups.Count - 1) then
  begin
    grp := Groups[AGroup];
    if not grp.UseDefaultAppearance then
    begin
      def := False;
      case AKind of
        ikGroupTop:
        begin
          ACanvas.Pen.Assign(grp.TopStroke);
          br := grp.TopFill;
          ACanvas.Brush.Assign(br);
        end;
        ikGroupBottom:
        begin
          ACanvas.Pen.Assign(grp.BottomStroke);
          br := grp.BottomFill;
          ACanvas.Brush.Assign(br);
        end;
      end;
    end;
  end;

  if def then
  begin
    case AKind of
      ikGroupTop:
      begin
        ACanvas.Pen.Assign(GroupsAppearance.TopStroke);
        br := GroupsAppearance.TopFill;
        ACanvas.Brush.Assign(br);
      end;
      ikGroupBottom:
      begin
        ACanvas.Pen.Assign(GroupsAppearance.BottomStroke);
        br := GroupsAppearance.BottomFill;
        ACanvas.Brush.Assign(br);
      end;
    end;
  end;

  FixStroke(ACanvas);

  b := True;
  df := True;
  DoBeforeDrawGroup(ACanvas, ARect, AGroup, AStartColumn, AEndColumn, AKind, b, df);

  if b then
  begin
    if df then
    begin
      drawr := Rect(Floor(ARect.Left), Floor(ARect.Top), Round(ARect.Right + 0.5), Round(ARect.Bottom + 0.5));

      if Assigned(br) and (br.Kind = tvbkGradient) then
      begin
        DrawGradient(ACanvas, br.Gradient.Color, br.Gradient.ColorTo, 32, drawr, False);
        ACanvas.Brush.Style := bsClear;
      end;

      ACanvas.Rectangle(drawr);
    end;

    sttxt := SaveStateEx(ACanvas);
    if def then
    begin
      case AKind of
        ikGroupTop:
        begin
          ACanvas.Font.Assign(GroupsAppearance.TopFont);
          ACanvas.Font.Color := GroupsAppearance.TopFontColor;
        end;
        ikGroupBottom:
        begin
          ACanvas.Font.Assign(GroupsAppearance.BottomFont);
          ACanvas.Font.Color := GroupsAppearance.BottomFontColor;
        end;
      end;
    end
    else if Assigned(grp) then
    begin
      case AKind of
        ikGroupTop:
        begin
          ACanvas.Font.Assign(grp.TopFont);
          ACanvas.Font.Color := grp.TopFontColor;
        end;
        ikGroupBottom:
        begin
          ACanvas.Font.Assign(grp.BottomFont);
          ACanvas.Font.Color := grp.BottomFontColor;
        end;
      end;
    end;

    txtr := ARect;
    InflateRectEx(txtr, -2, -2);

    str := GetGroupText(AGroup);
    DoGetGroupText(AGroup, AKind, str);
    b := True;
    DoBeforeDrawGroupText(ACanvas, txtr, AGroup, AStartColumn, AEndColumn, AKind, str, b);
    if b then
    begin
      case AKind of
        ikGroupTop:
        begin
          if GroupsAppearance.TopVerticalText then
            DrawText(ACanvas, txtr, GroupsAppearance.TopHorizontalTextAlign, GroupsAppearance.TopVerticalTextAlign, str, tvttNone, -90, False, True)
          else
            DrawText(ACanvas, txtr, GroupsAppearance.TopHorizontalTextAlign, GroupsAppearance.TopVerticalTextAlign, str, tvttNone, 0, False, True);
        end;
        ikGroupBottom:
        begin
          if GroupsAppearance.BottomVerticalText then
            DrawText(ACanvas, txtr, GroupsAppearance.TopHorizontalTextAlign, GroupsAppearance.TopVerticalTextAlign, str, tvttNone, 90, False, True)
          else
            DrawText(ACanvas, txtr, GroupsAppearance.TopHorizontalTextAlign, GroupsAppearance.TopVerticalTextAlign, str, tvttNone, 0, False, True);
        end;
      end;
      DoAfterDrawGroupText(ACanvas, txtr, AGroup, AStartColumn, AEndColumn, AKind, str);
    end;

    RestoreStateEx(sttxt, ACanvas);
    DoAfterDrawGroup(ACanvas, ARect, AGroup, AStartColumn, AEndColumn, AKind);
  end;

  RestoreStateEx(st, ACanvas);
end;

procedure TAdvTreeView.DrawNodeColumns;
var
  st: TAdvTreeViewCanvasSaveState;
  b, df: Boolean;
  I: Integer;
  r: TRectF;
  x, w: Double;
  cr: TRectF;
  crcl: TRectF;
  hs: Double;
  c: TAdvTreeViewColumn;
  br: TAdvTreeViewBrush;
  drawr: TRect;
begin
  inherited;
  cr := GetContentRect;
  crcl := GetContentClipRect;
  hs := GetHorizontalScrollPosition;
  for I := 0 to ColumnCount - 1 do
  begin
    if (I >= 0) and (I <= Columns.Count - 1) then
    begin
      x := ColumnPositions[I] - hs;
      w := ColumnWidths[I];

      if (ColumnStroke.Color <> clNone) and (ColumnStroke.Style <> psClear) then
      begin
        Canvas.Brush.Style := bsClear;
        Canvas.Pen.Assign(ColumnStroke);
        st := SaveStateEx(Canvas);

        b := True;
        df := True;
        r := RectF(Int(x) + 0.5, Int(cr.Top) + 0.5, Int(x + w) + 0.5, Int(cr.Bottom) - 0.5);

        DoBeforeDrawColumn(Canvas, r, I, b, df);

        if b then
        begin
          if df then
          begin
            Canvas.Rectangle(Round(R.Left), Round(R.Top), Round(R.Right), Round(R.Bottom));
          end;
          DoAfterDrawColumn(Canvas, r, I);
        end;

        RestoreStateEx(st, Canvas);
      end;
    end;
  end;


  if NodeStructure.Count > 0 then
  begin
    for I := 0 to ColumnCount - 1 do
    begin
      if (I >= 0) and (I <= Columns.Count - 1) then
      begin
        c := Columns[I];
        x := ColumnPositions[I] - hs;
        w := ColumnWidths[I];

        st := SaveStateEx(Canvas);

        b := True;
        df := True;

        Canvas.Brush.Style := bsClear;
        Canvas.Pen.Assign(NodesAppearance.ColumnStroke);
        if Canvas.Pen.Color = clNone then
          Canvas.Pen.Style := psClear;

        br := nil;
        if not c.UseDefaultAppearance then
        begin
          Canvas.Pen.Assign(c.Stroke);
          if Canvas.Pen.Color = clNone then
            Canvas.Pen.Style := psClear;

          br := c.Fill;
          Canvas.Brush.Assign(br);
          if Canvas.Brush.Color = clNone then
            Canvas.Brush.Style := bsClear;
        end;

        r := RectF(Int(x) + 1.5, Int(crcl.Top) + 0.5, Int(x + w) + 0.5, Int(crcl.Bottom) - 0.5);

        DoBeforeDrawNodeColumn(Canvas, r, I, b, df);

        if b then
        begin
          if df then
          begin
            drawr := Rect(Floor(R.Left), Floor(R.Top), Round(R.Right + 0.5), Round(R.Bottom + 0.5));
            if Assigned(br) and (br.Kind = tvbkGradient) then
            begin
              DrawGradient(Canvas, br.Gradient.Color, br.Gradient.ColorTo, 32, drawr, False);
              Canvas.Brush.Style := bsClear;
            end;

            Canvas.Rectangle(drawr);
          end;
          DoAfterDrawNodeColumn(Canvas, r, I);
        end;

        RestoreStateEx(st, Canvas);
      end;
    end;
  end;
end;

procedure TAdvTreeView.DrawNode(ACanvas: TCanvas; ARect: TRectF;
  ANode: TAdvTreeViewVirtualNode; ACaching: Boolean);
var
  st, sttxt, stl: TAdvTreeViewCanvasSaveState;
  str: String;
  b, df: Boolean;
  txtr: TRectF;
  I: Integer;
  bmp: TAdvTreeViewBitmap;
  bmpnr: TRectF;
  expr, dexr: TRectF;
  AColor: TColor;
  sts, stp: Integer;
  bmpn: TAdvTreeViewIconBitmap;
  ha, va: TAdvTreeViewTextAlign;
  c: TAdvTreeViewColumn;
  ww: Boolean;
  trim: TAdvTreeViewTextTrimming;
  en: Boolean;
  colw: Double;
  chk: TAdvTreeViewNodeCheckType;
  chkr: TRectF;
  rsel, rfoc: TRectF;
  ck, ext, sel: Boolean;
  cl: TAdvTreeViewColumn;
  ns: Double;
  lr: TRectF;
  n, p, lp: TAdvTreeViewVirtualNode;
  dr, r: TRectF;
  br: TAdvTreeViewBrush;
  drawr: TRect;
  hth: HTHEME;
  dChecked: Cardinal;
  ThemeStyle: DWord;
  rc: TRect;
  hpen, holdpen: THandle;
  lb: TLogBrush;
begin
  inherited;
  if not Assigned(ANode) then
    Exit;

  st := SaveStateEx(ACanvas);
  en := True;
  DoIsNodeEnabled(ANode, en);
  ext := False;
  DoIsNodeExtended(ANode, ext);
  sel := IsVirtualNodeSelected(ANode);

  if en then
  begin
    if sel then
    begin
      if ext then
      begin
        br := NodesAppearance.ExtendedSelectedFill;
        ACanvas.Brush.Assign(br);
        ACanvas.Pen.Assign(NodesAppearance.ExtendedSelectedStroke);
      end
      else
      begin
        br := NodesAppearance.SelectedFill;
        ACanvas.Brush.Assign(br);
        ACanvas.Pen.Assign(NodesAppearance.SelectedStroke);
      end;

      AColor := ACanvas.Brush.Color;
      DoGetNodeSelectedColor(ANode, AColor);
      ACanvas.Brush.Color := AColor;
    end
    else
    begin
      if ext then
      begin
        br := NodesAppearance.ExtendedFill;
        ACanvas.Brush.Assign(br);
        ACanvas.Pen.Assign(NodesAppearance.ExtendedStroke);
      end
      else
      begin
        br := NodesAppearance.Fill;
        ACanvas.Brush.Assign(br);
        ACanvas.Pen.Assign(NodesAppearance.Stroke);
      end;

      AColor := ACanvas.Brush.Color;
      DoGetNodeColor(ANode, AColor);
      ACanvas.Brush.Color := AColor;
    end;
  end
  else
  begin
    if ext then
    begin
      br := NodesAppearance.ExtendedDisabledFill;
      ACanvas.Brush.Assign(br);
      ACanvas.Pen.Assign(NodesAppearance.ExtendedDisabledStroke);
    end
    else
    begin
      br := NodesAppearance.DisabledFill;
      ACanvas.Brush.Assign(br);
      ACanvas.Pen.Assign(NodesAppearance.DisabledStroke);
    end;

    AColor := ACanvas.Brush.Color;
    DoGetNodeDisabledColor(ANode, AColor);
    ACanvas.Brush.Color := AColor;
  end;

  FixStroke(ACanvas);

  df := True;
  b := True;
  DoBeforeDrawNode(ACanvas, ARect, ANode, b, df);

  if b then
  begin
    if df then
    begin
      rsel := ARect;
      if sel and not ext then
      begin
        case NodesAppearance.SelectionArea of
          tsaFull: rsel.Left := 1.5;
          tsaFromText:
          begin
            if Length(ANode.TextRects) > 0 then
              rsel.Left := ANode.TextRects[0].Left;
          end;
        end;
      end;

      if ACanvas.Brush.Color = clNone then
        ACanvas.Brush.Style := bsClear;

      drawr := Rect(Floor(rsel.Left), Floor(rsel.Top), Round(rsel.Right + 0.5), Round(rsel.Bottom + 0.5));
      if Assigned(br) and (br.Kind = tvbkGradient) then
      begin
        DrawGradient(ACanvas, br.Gradient.Color, br.Gradient.ColorTo, 32, drawr, False);
        ACanvas.Brush.Style := bsClear;
      end;

      ACanvas.Rectangle(drawr);

      if (ANode = FocusedVirtualNode) and Focused and NodesAppearance.ShowFocus then
      begin
        ACanvas.Pen.Style := psDot;
        ACanvas.Pen.Color := clGray;
        ACanvas.Brush.Style := bsClear;
        rfoc := rsel;
        InflateRectEx(rfoc, -2, -2);
        ACanvas.Rectangle(Round(rfoc.Left), Round(rfoc.Top), Round(rfoc.Right), Round(rfoc.Bottom));
      end;
    end;

    sts := 0;
    stp := ColumnCount - 1;
    if ext then
      stp := Min(0, ColumnCount - 1);

    for I := sts to stp do
    begin
      cl := nil;
      if (I >= 0) and (I <= Columns.Count - 1) then
        cl := Columns[I];

      if (I >= 0) and (I <= Length(ANode.ExpandRects) - 1) and NodesAppearance.ShowLines and (I = NodesAppearance.ExpandColumn) and (NodesAppearance.ExpandWidth > 0) then
      begin
        r := ANode.ExpandRects[I];
        stl := SaveStateEx(ACanvas);

        lb.lbColor := ColorToRGB(NodesAppearance.LineStroke.Color);
        lb.lbStyle := BS_SOLID;

        hpen := ExtCreatePen(PS_COSMETIC or PS_ALTERNATE, 1, lb, 0, nil);
        holdpen := SelectObject(ACanvas.Handle, hpen);

        ns := NodesAppearance.LevelIndent;
        lr := RectF(r.Left + NodesAppearance.ExpandWidth / 2, ARect.Top, r.Left + NodesAppearance.ExpandWidth / 2 + ns / 2, ARect.Bottom);

        ACanvas.MoveTo(Round(Int(lr.Left) + 0.5), Round(Int(lr.Top + lr.Height / 2) + 0.5));
        ACanvas.LineTo(Round(Int(lr.Right) + 0.5), Round(Int(lr.Top + lr.Height / 2) + 0.5));

        if VisibleNodes.Count > 1 then
        begin
          n := ANode;
          p := n.GetParent;

          dr := lr;
          while Assigned(p) do
          begin
            if (((n = ANode) and (n.Index <= p.Children - 1)) or ((n <> ANode) and (n.Index < p.Children - 1))) then
            begin
              if n.Index < p.Children - 1 then
              begin
                ACanvas.MoveTo(Round(Int(dr.Left) + 0.5), Round(Int(dr.Top) + 0.5));
                ACanvas.LineTo(Round(Int(dr.Left) + 0.5), Round(Int(dr.Bottom) + 0.5));
              end
              else
              begin
                ACanvas.MoveTo(Round(Int(dr.Left) + 0.5), Round(Int(dr.Top) + 0.5));
                ACanvas.LineTo(Round(Int(dr.Left) + 0.5), Round(Int(dr.Top + dr.Height / 2) + 0.5));
              end;
            end;

            n := p;
            p := n.GetParent;
            OffsetRectEx(dr, -ns, 0);
          end;

          if NodeStructure.Count - ANode.TotalChildren - 1 > 0 then
          begin
            p := ANode.GetParent;
            if not Assigned(p) then
            begin
              if (ANode.Row = NodeStructure.Count - ANode.TotalChildren - 1) then
              begin
                ACanvas.MoveTo(Round(Int(dr.Left) + 0.5), Round(Int(dr.Top) + 0.5));
                ACanvas.LineTo(Round(Int(dr.Left) + 0.5), Round(Int(dr.Top + dr.Height / 2) + 0.5));
              end
              else if ANode.Row = 0 then
              begin
                ACanvas.MoveTo(Round(Int(dr.Left) + 0.5), Round(Int(dr.Top + dr.Height / 2) + 0.5));
                ACanvas.LineTo(Round(Int(dr.Left) + 0.5), Round(Int(dr.Bottom) + 0.5));
              end
              else
              begin
                ACanvas.MoveTo(Round(Int(dr.Left) + 0.5), Round(Int(dr.Top) + 0.5));
                ACanvas.LineTo(Round(Int(dr.Left) + 0.5), Round(Int(dr.Bottom) + 0.5));
              end;
            end
            else if Assigned(p) then
            begin
              lp := p;
              p := p.GetParent;
              dr := lr;
              OffsetRectEx(dr, -ns, 0);
              while Assigned(p) do
              begin
                lp := p;
                p := p.GetParent;
                OffsetRectEx(dr, -ns, 0);
              end;

              if Assigned(lp) and (lp.Row < NodeStructure.Count - lp.TotalChildren - 1)  then
              begin
                ACanvas.MoveTo(Round(Int(dr.Left) + 0.5), Round(Int(dr.Top) + 0.5));
                ACanvas.LineTo(Round(Int(dr.Left) + 0.5), Round(Int(dr.Bottom) + 0.5));
              end;
            end;
          end;
        end;

        DeleteObject(SelectObject(ACanvas.Handle, holdpen));

        RestoreStateEx(stl, ACanvas);
      end;

      if Assigned(cl) and not cl.UseDefaultAppearance and not ext then
        ACanvas.Font.Assign(cl.Font)
      else
      begin
        if ext then
          ACanvas.Font.Assign(NodesAppearance.ExtendedFont)
        else
          ACanvas.Font.Assign(NodesAppearance.Font);
      end;

      colw := ColumnWidths[I];
      if (colw > 0) or ext then
      begin
        if en then
        begin
          if sel then
          begin
            if ext then
              AColor := NodesAppearance.ExtendedSelectedFontColor
            else
              AColor := NodesAppearance.SelectedFontColor;

            DoGetNodeSelectedTextColor(ANode, I, AColor);
            ACanvas.Font.Color := AColor;
          end
          else
          begin
            if Assigned(cl) and not cl.UseDefaultAppearance and not ext then
              AColor := cl.FontColor
            else
            begin
              if ext then
                AColor := NodesAppearance.ExtendedFontColor
              else
                AColor := NodesAppearance.FontColor;
            end;

            DoGetNodeSelectedTextColor(ANode, I, AColor);
            ACanvas.Font.Color := AColor;
          end;
        end
        else
        begin
          if ext then
            AColor := NodesAppearance.ExtendedDisabledFontColor
          else
            AColor := NodesAppearance.DisabledFontColor;

          DoGetNodeSelectedTextColor(ANode, I, AColor);
          ACanvas.Font.Color := AColor;
        end;

        if (I >= 0) and (I <= Length(ANode.CheckRects) - 1) then
        begin
          chk := tvntNone;
          DoGetNodeCheckType(ANode, I, chk);
          if chk <> tvntNone then
          begin
            ck := False;
            DoIsNodeChecked(ANode, I, ck);

            if ck then
            begin
              DChecked := DFCS_BUTTONCHECK or DFCS_CHECKED;
              ThemeStyle := CBS_CHECKEDNORMAL;
            end
            else
            begin
              DChecked := DFCS_BUTTONCHECK;
              ThemeStyle := CBS_UnCheckEDNORMAL;
            end;

            chkr := ANode.CheckRects[I];

            sttxt := SaveStateEx(ACanvas);

            b := True;
            DoBeforeDrawNodeCheck(ACanvas, chkr, I, ANode, b);
            if b then
            begin
              rc := Bounds(Round(chkr.Left + (chkr.Width - 16) / 2), Round(chkr.Top + (chkr.Height - 16) / 2), 16, 16);
              if FIsWinXP and IsThemeActive and not (csDesigning in ComponentState) then
              begin
                hth := OpenThemeData(Self.Handle,'button');
                DrawThemeBackground(hth, ACanvas.Handle, BP_CHECKBOX, ThemeStyle,@rc,nil);
                CloseThemeData(hth);
              end
              else
                DrawFrameControl(ACanvas.Handle,rc,DFC_BUTTON, DChecked);

              DoAfterDrawNodeCheck(ACanvas, chkr, I, ANode);
            end;

            RestoreStateEx(sttxt, ACanvas);
          end;
        end;

        if (I >= 0) and (I <= Length(ANode.BitmapRects) - 1) then
        begin
          bmpn := nil;
          DoGetNodeIcon(ANode, I, IsRetina, bmpn);
          if Assigned(bmpn) then
          begin
            bmpnr := ANode.BitmapRects[I];
            sttxt := SaveStateEx(ACanvas);

            b := True;
            DoBeforeDrawNodeIcon(ACanvas, bmpnr, I, ANode, bmpn, b);
            if b then
            begin
              if Assigned(bmpn) then
                ACanvas.StretchDraw(Rect(Round(bmpnr.Left), Round(bmpnr.Top), Round(bmpnr.Right), Round(bmpnr.Bottom)), bmpn);
              DoAfterDrawNodeIcon(ACanvas, bmpnr, I, ANode, bmpn);
            end;

            RestoreStateEx(sttxt, ACanvas);
          end;
        end;

        if (I >= 0) and (I <= Length(ANode.ExpandRects) - 1) and (ANode.Children > 0) and (I = NodesAppearance.ExpandColumn) and (NodesAppearance.ExpandWidth > 0) then
        begin
          if IsRetina then
          begin
            if ANode.Expanded then
              bmp := NodesAppearance.CollapseNodeIconLarge
            else
              bmp := NodesAppearance.ExpandNodeIconLarge;
          end
          else
          begin
            if ANode.Expanded then
              bmp := NodesAppearance.CollapseNodeIcon
            else
              bmp := NodesAppearance.ExpandNodeIcon;
          end;

          expr := ANode.ExpandRects[I];

          sttxt := SaveStateEx(ACanvas);

          b := True;
          DoBeforeDrawNodeExpand(ACanvas, expr, I, ANode, bmp, b);
          if b then
          begin
            if Assigned(bmp) then
            begin
              if IsRetina then
              begin
                dexr := RectF(Int(expr.Left + (expr.Width - bmp.Width / 2) / 2), Int(expr.Top + (expr.Height - bmp.Height / 2) / 2),
                  Int(expr.Left + (expr.Width - bmp.Width / 2) / 2) + bmp.Width / 2, Int(expr.Top + (expr.Height - bmp.Height / 2) / 2) + bmp.Height / 2);
              end
              else
              begin
                dexr := RectF(Int(expr.Left + (expr.Width - bmp.Width) / 2), Int(expr.Top + (expr.Height - bmp.Height) / 2),
                  Int(expr.Left + (expr.Width - bmp.Width) / 2) + bmp.Width, Int(expr.Top + (expr.Height - bmp.Height) / 2) + bmp.Height);
              end;
              ACanvas.StretchDraw(Rect(Round(dexr.Left), Round(dexr.Top), Round(dexr.Right), Round(dexr.Bottom)), bmp.Graphic);
            end;
            DoAfterDrawNodeExpand(ACanvas, expr, I, ANode, bmp);
          end;

          RestoreStateEx(sttxt, ACanvas);
        end;

        if (I >= 0) and (I <= Length(ANode.TextRects) - 1) then
        begin
          txtr := ANode.TextRects[I];
          InflateRectEx(txtr, -2, 0);

          c := nil;
          if (I <= Columns.Count - 1) then
            c := Columns[I];

          ha := tvtaLeading;
          va := tvtaCenter;
          ww := False;
          trim := tvttNone;
          if Assigned(c) then
          begin
            ha := c.HorizontalTextAlign;
            va := c.VerticalTextAlign;
            ww := c.WordWrapping;
            trim := c.Trimming;
          end;

          DoGetNodeTrimming(ANode, I, trim);
          DoGetNodeWordWrapping(ANode, I, ww);
          DoGetNodeHorizontalTextAlign(ANode, I, ha);
          DoGetNodeVerticalTextAlign(ANode, I,va);

          str := '';
          DoGetNodeText(ANode, I, tntmDrawing, str);
          sttxt := SaveStateEx(ACanvas);

          b := True;
          DoBeforeDrawNodeText(ACanvas, txtr, I, ANode, str, b);
          if b then
          begin
            if InplaceEditorActive and ((UpdateNodeColumn <> I) or (FocusedVirtualNode <> ANode)) or not InplaceEditorActive then
              DrawText(ACanvas, txtr, ha, va, str, trim, 0, False, True, False, ww);
            DoAfterDrawNodeText(ACanvas, txtr, I, ANode, str);
          end;

          RestoreStateEx(sttxt, ACanvas);
        end;
      end;
    end;

    DoAfterDrawNode(ACanvas, ARect, ANode);
  end;

  RestoreStateEx(st, ACanvas);
end;

function TAdvTreeView.DrawText(ACanvas: TCanvas; ARect: TRectF;
  AHorizontalAlign, AVerticalAlign: TAdvTreeViewTextAlign; AText: String;
  ATrimming: TAdvTreeViewTextTrimming; AAngle: Single; AReverseAlignment,
  ASupportHTML, ATestAnchor, AWordWrapping: Boolean; AX, AY, AMinWidth,
  AMinHeight: Single): String;
var
  a, s: String;
  fa: String;
  XSize, YSize: Integer;
  hl, ml: Integer;
  hr: TRect;
  xs, ys: Single;
  st: TAdvTreeViewCanvasSaveState;
  htmlr: TRect;
  isanchor: boolean;
  r: TRect;
  dstyle: DWORD;
  rcalc: TRectF;
begin
  inherited;

  ACanvas.Brush.Style := bsClear;

  if (AMinHeight > -1) and (ARect.Height < AMinHeight) then
    ARect.Height := AMinHeight;

  if (AMinWidth > -1) and (ARect.Width < AMinWidth) then
    ARect.Width := AMinWidth;

  r := Rect(Round(ARect.Left), Round(ARect.Top), Round(ARect.Right), Round(ARect.Bottom));

  Result := '';

  if ((AnsiPos('</', AText) > 0) or (AnsiPos('/>', AText)  > 0) or (AnsiPos('<BR>', UpperCase(AText)) > 0)) and ASupportHTML then
  begin
    HTMLDrawEx(ACanvas, AText, r, nil, 0, 0,-1,-1,0,False,True,False,False,False,False,AWordWrapping,1.0,clBlue,
        clNone,clNone,clNone,a,s,fa,XSize,YSize,hl,ml,hr, PictureCache, PictureContainer, 0);

    xs := ARect.Left;
    ys := ARect.Top;

    case AHorizontalAlign of
      tvtaCenter: xs := xs + (ARect.Width - XSize) / 2;
      tvtaTrailing: xs := ARect.Left + ARect.Width - XSize;
    end;

    case AVerticalAlign of
      tvtaCenter: ys := ys + (ARect.Height - YSize) / 2;
      tvtaTrailing: ys := ys + ARect.Height - YSize;
    end;

    htmlr := Rect(Round(xs), Round(ys), Round(xs + XSize), Round(ys + YSize));

    st := SaveStateEx(ACanvas);
    IntersectClipRectEx(ACanvas,st, ARect);
    isanchor := HTMLDrawEx(ACanvas, AText, htmlr, nil, Round(AX), Round(AY),-1,-1,0,ATestAnchor,False,False,False,False,False,AWordWrapping,1.0,clBlue,
        clNone,clNone,clNone,a,s,fa,XSize,YSize,hl,ml,hr, PictureCache, PictureContainer, 0);

    RestoreStateEx(st, ACanvas);

    if isanchor then
      Result := a;
  end
  else if not ATestAnchor then
  begin
    if AWordWrapping then
      dstyle := DT_WORDBREAK
    else
      dstyle := DT_SINGLELINE;

    case AHorizontalAlign of
      tvtaCenter: dstyle := dstyle or DT_CENTER;
      tvtaLeading: dstyle := dstyle or DT_LEFT;
      tvtaTrailing: dstyle := dstyle or DT_RIGHT;
    end;

    case AVerticalAlign of
      tvtaCenter: dstyle := dstyle or DT_VCENTER;
      tvtaLeading: dstyle := dstyle or DT_TOP;
      tvtaTrailing: dstyle := dstyle or DT_BOTTOM;
    end;

    case ATrimming of
      tvttCharacter: dstyle := dstyle or DT_END_ELLIPSIS;
      tvttWord: dstyle := dstyle or DT_WORD_ELLIPSIS;
    end;

    if AWordWrapping then
    begin
      rcalc := CalculateText(ACanvas, AText, AWordWrapping, ARect);
      r.Top := r.Top +((r.Bottom - r.Top) - Round(rcalc.Height)) div 2;
      r.Bottom := r.Top + Round(rcalc.Height)
    end;

    Windows.DrawText(ACanvas.Handle, PChar(AText), Length(AText), r, dstyle);
  end;
end;

procedure TAdvTreeView.FixStroke(ACanvas: TCanvas);
begin
  if ((ACanvas.Pen.Color = clNone) or (ACanvas.Pen.Style = psClear)) then
  begin
    if (ACanvas.Brush.Style <> bsClear) and (ACanvas.Brush.Color <> clNone) then
    begin
      ACanvas.Pen.Color := ACanvas.Brush.Color;
      ACanvas.Pen.Style := psSolid;
    end;
  end;
end;

function TAdvTreeView.GetVersion: string;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

procedure TAdvTreeView.KeyDown(var Key: Word; Shift: TShiftState);
var
  k: Char;
begin
  inherited;
  k := #0;
  HandleKeyDown(Key, k, Shift);
end;

procedure TAdvTreeView.KeyUp(var Key: Word; Shift: TShiftState);
var
  k: Char;
begin
  inherited;
  k := #0;
  HandleKeyUp(Key, k, Shift);
end;

procedure TAdvTreeView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  HandleMouseDown(Button, Shift, X, Y);
end;

procedure TAdvTreeView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  HandleMouseMove(Shift, X, Y);
end;

procedure TAdvTreeView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  HandleMouseUp(Button, Shift, X, Y);
end;

procedure TAdvTreeView.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  if TabStop then
    Message.result := DLGC_WANTALLKEYS or DLGC_WANTARROWS
  else
    Message.result := 0;
end;

procedure TAdvTreeView.WMMouseWheel(var Message: TWMMouseWheel);
var
  b: Boolean;
begin
  inherited;
  HandleMouseWheel([], Message.WheelDelta, b);
end;

end.
