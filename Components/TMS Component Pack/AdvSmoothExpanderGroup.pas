{*************************************************************************}
{ TAdvSmoothExpanderGroup component                                       }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{            copyright © 2015                                             }
{            Email : info@tmssoftware.com                                 }
{            Website : http://www.tmssoftware.com/                        }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvSmoothExpanderGroup;

{$I TMSDEFS.INC}

{$DEFINE REMOVEDRAW}

interface

uses
  Windows, Forms, Classes, Graphics, Messages, Controls, stdctrls,
  Sysutils, AdvSmoothExpanderButtonPanel, GDIPFill, AdvStyleIF, Types,
  AdvGDIP
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First Release
  // v1.0.1.0 : New : Support for Windows Vista and Windows Seven Style
  // v1.0.2.0 : New : Built-in support for reduced color set for use with terminal servers
  // v1.0.3.0 : New : Delphi 2010 Touch Support
  // v1.0.3.1 : Improved : Exposed property Anchors
  // v1.0.3.2 : Fixed : Issue with accessing other controls
  // v1.0.4.0 : New : Built-in support for Office 2010 colors
  // v1.1.0.0 : New : Metro Style support
  // v1.2.0.0 : New : Windows 10, Office 2016 styles added

type
  TAdvSmoothExpanderGroup = class;

  TCustomAdvSmoothExpanderButtonPanel = class(TAdvSmoothExpanderButtonPanel)
  private
    FMouseDown: Boolean;
    FHideList: TList;
    FTopLeft: TPoint;
    FOldWidth, FOldHeight: Integer;
    FWidthHeight: TPoint;
    FIndex: Integer;
    function GetHeightEx: integer;
    procedure SetHeightEx(const Value: integer);
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMSize(var Msg: TMessage); message WM_SIZE;
    procedure WMEnterSizeMove(var Msg: TMessage); message WM_ENTERSIZEMOVE;
    procedure WMExitSizeMove(var Msg: TMessage); message WM_EXITSIZEMOVE;
    procedure WMPaint(var Msg: TWMPAINT); message WM_PAINT;
    procedure SetIndex(const Value: Integer);
    function GetWidthEx: Integer;
    procedure SetWidthEx(Value: Integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CreateWnd; override;
    procedure Synchronize;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Index: Integer read FIndex write SetIndex;
    property Height: Integer read GetHeightEx write SetHeightEx;
    property Width: Integer read GetWidthEx write SetWidthEx;
  end;

//  TAdvSmoothExpanderGroupStyle = (gsVertical, gsHorizontal);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothExpanderGroup = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FStyle: TTMSStyle;
    FDesignTime: Boolean;
    FEnableResize: Boolean;
    FUpdateCount: Integer;
    FHorzPadding: Integer;
    FVertPadding: Integer;
    FIsArranging: Boolean;
    FScrollBar: TScrollBar;
    FPanels: TList;
    FCode: Boolean;
    FColumns: integer;
    FOldWidth, FOldHeight: integer;
    FFill: TGDIPFill;
//    FGroupStyle: TAdvSmoothExpanderGroupStyle;
    procedure WMSize(var Msg: TMessage); message WM_SIZE;
    procedure SetHorzPadding(const Value: Integer);
    procedure SetVertPadding(const Value: Integer);
    function GetPanel(Index: Integer): TCustomAdvSmoothExpanderButtonPanel;
    procedure SetPanel(Index: Integer; const Value: TCustomAdvSmoothExpanderButtonPanel);
    procedure SetColumns(const Value: Integer);
    function GetPanelCount: Integer;
    procedure SetFill(const Value: TGDIPFill);
//    procedure SetGroupStyle(const Value: TAdvSmoothExpanderGroupStyle);
  protected
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure ArrangeControlsVert;
    procedure ArrangeControlsHorz;
    procedure ArrangeControls;
    procedure Scroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    function PanelHeights: Integer;
    function PanelWidths: Integer;
    procedure UpdateScrollbar;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure FillChanged(Sender: TObject);
    property Style: TTMSStyle read FStyle write FStyle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure ChildPanelChanged(APanel: TCustomAdvSmoothExpanderButtonPanel);
    property IsArranging: Boolean read FIsArranging;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure UpdateGroup;
    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure InitPanels;
    function AddPanel: TCustomAdvSmoothExpanderButtonPanel;
    function AppendPanel: TCustomAdvSmoothExpanderButtonPanel;
    function InsertPanel(Index: Integer): TCustomAdvSmoothExpanderButtonPanel;
    procedure RemovePanel(Index: Integer);
    procedure MovePanel(FromIndex,ToIndex: Integer);
    property PanelCount: Integer read GetPanelCount;
    property Panels[Index: Integer]: TCustomAdvSmoothExpanderButtonPanel read GetPanel write SetPanel;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
    procedure SaveToTheme(FileName: String);
    procedure LoadFromTheme(FileName: String);
    function GetThemeID: String;
  published
    property Columns: Integer read FColumns write SetColumns default 1;
    property HorzPadding: Integer read FHorzPadding write SetHorzPadding;
    property VertPadding: Integer read FVertPadding write SetVertPadding;
    property Fill: TGDIPFill read FFill write SetFill;
//    property GroupStyle: TAdvSmoothExpanderGroupStyle read FGroupStyle write SetGroupStyle default gsVertical;

    property Align;
    property Anchors;
    {$IFDEF DELPHI_TOUCH}
    property OnGesture;
    property Touch;
    {$ENDIF}
  end;

implementation

constructor TCustomAdvSmoothExpanderButtonPanel.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffered := True;
  FHideList := TList.Create;
end;

destructor TCustomAdvSmoothExpanderButtonPanel.Destroy;
begin
  FHideList.Free;
  inherited;
end;

procedure TCustomAdvSmoothExpanderButtonPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FMouseDown := false;
end;

procedure TCustomAdvSmoothExpanderButtonPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FMouseDown := true;
end;

procedure TCustomAdvSmoothExpanderButtonPanel.Loaded;
begin
  inherited;
  FTopLeft := ClientOrigin;
  FWidthHeight := Point(Width, Height);
end;

function TCustomAdvSmoothExpanderButtonPanel.GetHeightEx: integer;
begin
  Result := inherited Height;
end;

procedure TCustomAdvSmoothExpanderButtonPanel.SetHeightEx(const Value: integer);
begin
  inherited Height := Value;
end;

function TCustomAdvSmoothExpanderButtonPanel.GetWidthEx: Integer;
begin
  Result := inherited Width;
end;

procedure TCustomAdvSmoothExpanderButtonPanel.SetWidthEx(Value: Integer);
begin
  inherited Width := Value;
end;

procedure TCustomAdvSmoothExpanderButtonPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TCustomAdvSmoothExpanderButtonPanel.WMNCHitTest(var Msg: TWMNCHitTest);
var
  r: TRect;
  pt: TPoint;
begin
  inherited;
  if (csDesigning in ComponentState) then Exit;

  r := ClientRect;

  pt := ScreenToClient(point(msg.xpos,msg.ypos));

  MouseMove([],pt.X,pt.Y);
  if (pt.Y < r.Top + 35) and not Self.PtInGPRect(GetExpanderRect, pt) then
    Msg.Result := HTCAPTION;

  SetWindowPos(Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);

  if (Msg.Result = htClient) then
  begin
    if (pt.y > Height - GetSystemMetrics(SM_CYSIZEFRAME) - 2) and (pt.x > width - GetSystemMetrics(SM_CXSIZEFRAME) - 2) then
    begin
      SetWindowPos(Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
      Msg.Result := HTBOTTOMRIGHT;
    end

    else if (pt.y > height - GetSystemMetrics(SM_CYSIZEFRAME)) then
    begin
      SetWindowPos(Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
      Msg.Result := HTBOTTOM;
    end
    else if (pt.x > width - GetSystemMetrics(SM_CXSIZEFRAME)) then
    begin
      SetWindowPos(Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
      Msg.Result := HTRIGHT;
    end;
  end;
end;

procedure TCustomAdvSmoothExpanderButtonPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TCustomAdvSmoothExpanderButtonPanel.WMSize(var Msg: TMessage);
begin
  inherited;
end;

procedure TCustomAdvSmoothExpanderButtonPanel.WMEnterSizeMove(var Msg: TMessage);
begin
  FTopLeft := ClientOrigin;
  FWidthHeight := Point(Width, Height);
end;

procedure TCustomAdvSmoothExpanderButtonPanel.WMExitSizeMove(var Msg: TMessage);
begin
  inherited;
  Synchronize;
end;

procedure TCustomAdvSmoothExpanderButtonPanel.CreateWnd;
var
  APG: TAdvSmoothExpanderGroup;
begin
  inherited;
  if (Parent is TAdvSmoothExpanderGroup) and (csDesigning in ComponentState) then
  begin
    APG := Parent as TAdvSmoothExpanderGroup;

//    if APG.FGroupStyle = gsVertical then
//    begin
      Width := APG.Width - 2*APG.HorzPadding;
      Left := APG.HorzPadding;
      if Top < APG.VertPadding then
        Top := APG.VertPadding;
//    end
//    else
//    begin
//      Height := APG.Height - 2*APG.HorzPadding;
//      Top := APG.VertPadding;
//      if Left < APG.HorzPadding then
//        Left := APG.HorzPadding;
//    end;
  end;
end;


procedure TCustomAdvSmoothExpanderButtonPanel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  i: Integer;
begin
  if (Parent is TAdvSmoothExpanderGroup) then
    SetResizeEnabled((Parent as TAdvSmoothExpanderGroup).FEnableResize);
  inherited;
  Synchronize;

  for i := 1 to ControlCount do
  begin
    if Controls[i - 1] is TCustomAdvSmoothExpanderButtonPanel then
      TCustomAdvSmoothExpanderButtonPanel(Controls[i - 1]).Changed;
  end;
  Invalidate;

  FOldWidth := AWidth;
  FOldHeight := AHeight;
  SetResizeEnabled(true);
end;

procedure TCustomAdvSmoothExpanderButtonPanel.Synchronize;
begin
  if (Parent is TAdvSmoothExpanderGroup) then
    with (Parent as TAdvSmoothExpanderGroup) do
      ChildPanelChanged(Self);
end;

procedure TCustomAdvSmoothExpanderButtonPanel.WMPaint(var Msg: TWMPAINT);
var
  DC: HDC;
begin
  DC := GetDC(Handle);
  Canvas.Handle := DC;
  Canvas.Brush.Color := clWhite;
  ReleaseDC(Handle,DC);
  inherited;
end;

procedure TCustomAdvSmoothExpanderButtonPanel.SetIndex(const Value: Integer);
begin
  FIndex := Value;
end;

procedure TCustomAdvSmoothExpanderButtonPanel.Assign(Source: TPersistent);
begin
  //
end;

{ TAdvSmoothExpanderGroup }

procedure TAdvSmoothExpanderGroup.ArrangeControlsVert;
var
  PL: TList;
  i, j, T1,T2,H: Integer;
  Sorted: Boolean;
  S: Pointer;

begin
  if FUpdateCount > 0 then
    Exit;

  inc(FUpdateCount);

  PL := TList.Create;

  for i := 1 to ControlCount do
  begin
    if (Controls[i - 1] is TCustomAdvSmoothExpanderButtonPanel) then
      PL.Add(Controls[i - 1]);
  end;

  Sorted := false;

  // perform a simple bubble sort, nr. of subpanels should be small

  while not Sorted do
  begin
    Sorted := True;
    for i := 2 to PL.Count do
    begin
      if FCode then
      begin
        T1 := TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 2]).Index;
        T2 := TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 1]).Index;
      end
      else
      begin
        T1 := TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 2]).Top;
        T2 := TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 1]).Top;
      end;

      if T1 > T2 then
      begin
        Sorted := False;
        S := PL.Items[i - 2];
        PL.Items[i - 2] := PL.Items[i - 1];
        PL.Items[i - 1] := S;
      end;
    end;
  end;

  H := VertPadding;

  if Assigned(FScrollBar) then
  begin
    if FScrollBar.Visible then
      H := VertPadding - FScrollBar.Position;
  end;

  for i := 1 to PL.Count do
  begin
    TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 1]).Top := H;

    if FScrollBar.Visible then
    begin
      TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 1]).Left := HorzPadding + ((Width - FScrollBar.Width - HorzPadding) div Columns) * ((i - 1) mod Columns);
      TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 1]).Width := (Width - 2* HorzPadding - FScrollBar.Width) div Columns
    end
    else
    begin
      TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 1]).Left := HorzPadding + ((Width - HorzPadding) div Columns) * ((i - 1) mod Columns);
      TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 1]).Width := (Width - 2* HorzPadding) div Columns;
    end;

    if TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 1]).Visible then
      if (i mod Columns = 0) then
        H := H + TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 1]).Height + VertPadding;
  end;

  for i := 1 to PL.Count do
  begin
    TAdvsmoothExpanderButtonPanel(PL.Items[i - 1]).Invalidate;

    for j := 1 to TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 1]).ControlCount do
    begin

      if TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 1]).Controls[j - 1] is TWinControl then
      begin
        if (TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 1]).Controls[j - 1] as TWinControl).Visible and
           (TWinControl(TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 1]).Controls[j - 1]).ControlCount = 0) then
        with TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 1]).Controls[j - 1] as TWinControl do
        begin
          SendMessage(Handle, WM_SETREDRAW, Integer(False), 0);
          SendMessage(Handle, WM_SETREDRAW, Integer(True), 0);
        end;
      end;

      with TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 1]).Controls[j - 1] do
      begin
        // Invalidate;
        // force a full repaint, including borders
        Width := Width + 1;
        Width := Width - 1;
      end;
    end;
  end;


  PL.Free;
  dec(FUpdateCount);
  Invalidate;
end;

procedure TAdvSmoothExpanderGroup.ArrangeControlsHorz;
var
  PL: TList;
  i, T1,T2,H: Integer;
  Sorted: Boolean;
  S: Pointer;

begin
  PL := TList.Create;

  for i := 1 to ControlCount do
  begin
    if (Controls[i - 1] is TCustomAdvSmoothExpanderButtonPanel) then
      PL.Add(Controls[i - 1]);
  end;

  Sorted := False;

  // perform a simple bubble sort, nr. of subpanels should be small

  while not Sorted do
  begin
    Sorted := True;
    for i := 2 to PL.Count do
    begin
      T1 := TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 2]).Left;
      T2 := TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 1]).Left;
      if T1 > T2 then
      begin
        Sorted := False;
        S := PL.Items[i - 2];
        PL.Items[i - 2] := PL.Items[i - 1];
        PL.Items[i - 1] := S;
      end;
    end;
  end;

  H := HorzPadding;

  if Assigned(FScrollBar) then
  begin
  if not FScrollBar.Visible then
    H := HorzPadding
  else
    H := HorzPadding - FScrollBar.Position;
  end;

  for i := 1 to PL.Count do
  begin
    TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 1]).Left := H;
    TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 1]).Top := VertPadding;

    if FScrollBar.Visible then
      TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 1]).Height := Height - 2* VertPadding - FScrollBar.Height
    else
      TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 1]).Height := Height - 2* VertPadding;

    if TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 1]).Visible then
      H := H + TCustomAdvSmoothExpanderButtonPanel(PL.Items[i - 1]).Width + HorzPadding;
  end;

  PL.Free;
end;

procedure TAdvSmoothExpanderGroup.ArrangeControls;
begin
  if (csLoading in ComponentState) then
    Exit;

  if FIsArranging then
    Exit;
  if FUpdateCount > 0 then
    Exit;

  FIsArranging := True;
//  if GroupStyle = gsVertical then
    ArrangeControlsVert;
//  else
//    ArrangeControlsHorz;
  FIsArranging := False;
end;

procedure TAdvSmoothExpanderGroup.ChildPanelChanged(APanel: TCustomAdvSmoothExpanderButtonPanel);
begin
  UpdateScrollBar;
  ArrangeControls;
end;

constructor TAdvSmoothExpanderGroup.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  ControlStyle := ControlStyle - [csSetCaption];
  FScrollBar := TScrollBar.Create(nil);
  FScrollBar.Parent := Self;
  FScrollBar.Align := alRight;

  FScrollBar.Position := 0;
  FScrollBar.Kind := sbVertical;
  FScrollBar.OnScroll := Scroll;
  FScrollBar.Visible := False;
  FScrollBar.LargeChange := 20;
  FScrollBar.SmallChange := 2;
  FScrollBar.TabStop := False;

  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;

  FColumns := 1;
  FHorzPadding := 8;
  FVertPadding := 8;
//  FGroupStyle := gsVertical;
  FPanels := TList.Create;

  DoubleBuffered := true;

  Width := 250;
  Height := 350;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
  if FDesignTime then
    SetComponentStyle(tsOffice2007Luna);
end;

procedure TAdvSmoothExpanderGroup.SetHorzPadding(const Value: Integer);
begin
  if FHorzPadding <> Value then
  begin
    FHorzPadding := Value;
    ArrangeControls;
  end;
end;

procedure TAdvSmoothExpanderGroup.SetVertPadding(const Value: Integer);
begin
  if FVertPadding <> Value then
  begin
    FVertPadding := Value;
    ArrangeControls;
  end;
end;

procedure TAdvSmoothExpanderGroup.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  doarr: boolean;
begin
  FEnableResize := true;
  doarr := (AWidth <> FOldWidth) or (AHeight <> FOldHeight);
  inherited;
  if doarr then
    ArrangeControls;
  FOldWidth := AWidth;
  FOldHeight := AHeight;
  FEnableResize := false;
end;

destructor TAdvSmoothExpanderGroup.Destroy;
var
  i: Integer;
begin
  for i := 1 to FPanels.Count do
    TAdvsmoothExpanderButtonPanel(FPanels[i - 1]).Free;
  FPanels.Free;
  FScrollBar.Free;
  FFill.Free;
  inherited;
end;

procedure TAdvSmoothExpanderGroup.SaveToTheme(FileName: String);
begin

end;

procedure TAdvSmoothExpanderGroup.Scroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if ScrollCode = scEndScroll then
  begin
    if FScrollbar.Position > FScrollbar.Max - FScrollbar.Pagesize then
      FScrollbar.Position := FScrollbar.Max - FScrollbar.Pagesize;
    ArrangeControls;
  end;
end;

procedure TAdvSmoothExpanderGroup.Paint;
var
  g: TGPGraphics;
begin
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  FFill.Fill(g, MakeRect(0, 0, Width - 1, Height - 1));
  g.Free;
end;

function TAdvSmoothExpanderGroup.PanelHeights: Integer;
var
  H, i: Integer;
begin
  H := VertPadding;

  for i := 1 to ControlCount do
  begin
    if (Controls[i - 1] is TAdvsmoothExpanderButtonPanel) then
      if TAdvsmoothExpanderButtonPanel(Controls[i - 1]).Visible then
        if (i mod Columns = 0) then
          H := H + TAdvsmoothExpanderButtonPanel(Controls[i - 1]).Height + VertPadding;
  end;

  Result := H;
end;

function TAdvSmoothExpanderGroup.PanelWidths: Integer;
var
  W, i: Integer;
begin
  W := HorzPadding;
  for i := 1 to ControlCount do
  begin
    if (Controls[i - 1] is TAdvsmoothExpanderButtonPanel) then
      if TAdvsmoothExpanderButtonPanel(Controls[i - 1]).Visible then
        W := W + TAdvsmoothExpanderButtonPanel(Controls[i - 1]).Width +  HorzPadding;
  end;

  Result := W;
end;

procedure TAdvSmoothExpanderGroup.Loaded;
begin
  inherited;
  // force correct initialization
  Width := Width + 1;
  Width := Width - 1;

  UpdateScrollBar;
  ArrangeControls;
  InitPanels;
end;

procedure TAdvSmoothExpanderGroup.LoadFromTheme(FileName: String);
begin

end;

procedure TAdvSmoothExpanderGroup.UpdateScrollbar;
var
  TH: Integer;
begin
  if FUpdateCount > 0 then
    Exit;

//  if GroupStyle = gsVertical then
//  begin
    if FScrollBar.Kind = sbHorizontal then
    begin
      FScrollBar.Kind := sbVertical;
    end;

    FScrollBar.Width := 16;
    TH := PanelHeights;
    if (TH > Height) and (TH > 0) then
    begin
      if FScrollBar.Position > TH then
        FScrollBar.Position := TH;
      FScrollBar.PageSize := Height;
      FScrollBar.LargeChange := Height div 3;
      FScrollBar.Max := TH;
    end;
    FScrollBar.Visible := TH > Height;
    FScrollBar.Invalidate;
//  end
//  else
//  begin
//    if FScrollBar.Kind = sbVertical then
//      FScrollBar.Kind := sbHorizontal;
//
//    if FScrollBar.Align = alRight then
//    begin
//      FScrollBar.Align := alBottom;
//    end;
//    FScrollBar.Height := 16;
//
//    TH := PanelWidths;
//    if TH > Width then
//    begin
//      if FScrollBar.Position > TH then
//        FScrollBar.Position := TH;
//      FScrollBar.PageSize := Width;
//      FScrollBar.LargeChange := Width div 3;
//      FScrollBar.Max := TH;
//    end;
//
//    FScrollBar.Visible := TH > Width;
//    FScrollBar.Invalidate;
//  end;
end;

procedure TAdvSmoothExpanderGroup.WMSize(var Msg: TMessage);
begin
  inherited;
  UpdateScrollBar;
end;

procedure TAdvSmoothExpanderGroup.UpdateGroup;
begin
  ArrangeControls;
  UpdateScrollBar;
  Height := Height + 1;
  Height := Height - 1;
end;

procedure TAdvSmoothExpanderGroup.Clear;
var
  i: Integer;
begin
  i := 0;
  while (i < ControlCount) do
  begin
    if (Controls[i] is TAdvsmoothExpanderButtonPanel) then
      Controls[i].Free
    else
      inc(i);
  end;
  FPanels.Clear;
  UpdateScrollbar;
end;


procedure TAdvSmoothExpanderGroup.InitPanels;
var
  i: Integer;
begin
  i := 0;
  FPanels.Clear;
  while (i < ControlCount) do
  begin
    if (Controls[i] is TAdvsmoothExpanderButtonPanel) then
    begin
      FPanels.Add(Pointer(Controls[i]));
    end;
    inc(i);
  end;
end;

function TAdvSmoothExpanderGroup.AddPanel: TCustomAdvSmoothExpanderButtonPanel;
begin
  Result := TCustomAdvSmoothExpanderButtonPanel.Create(Self);
  Result.Parent := Self;
  Result.Index := FPanels.Count;
  Result.SetComponentStyle(FStyle);
  FPanels.Add(Pointer(Result));
  UpdateScrollbar;
  ArrangeControls;
  UpdateScrollbar;
end;

function TAdvSmoothExpanderGroup.AppendPanel: TCustomAdvSmoothExpanderButtonPanel;
begin
  Result := AddPanel;
  if PanelCount > 1 then
  begin
    MovePanel(PanelCount - 1,0);
  end;
end;


function TAdvSmoothExpanderGroup.InsertPanel(Index: Integer): TCustomAdvSmoothExpanderButtonPanel;
var
  i: Integer;
begin
  Result := TCustomAdvSmoothExpanderButtonPanel.Create(Self);
  Result.Parent := Self;
  Result.Index := Index;
  Result.SetComponentStyle(FStyle);
  FPanels.Insert(Index,Pointer(Result));

  for i := 1 to FPanels.Count do
  begin
    TCustomAdvSmoothExpanderButtonPanel(FPanels[i - 1]).Index := i - 1;
  end;

  UpdateScrollBar;
  ArrangeControls;
  UpdateScrollBar;
end;

procedure TAdvSmoothExpanderGroup.RemovePanel(Index: Integer);
var
  i: Integer;
begin
  if (Index < FPanels.Count) and (Index >= 0) then
  begin
    TCustomAdvSmoothExpanderButtonPanel(FPanels[Index]).Free;
    FPanels.Delete(Index);

    for i := 1 to FPanels.Count do
    begin
      TCustomAdvSmoothExpanderButtonPanel(FPanels[i - 1]).Index := i - 1;
    end;
    ArrangeControls;
  end;
end;

function TAdvSmoothExpanderGroup.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothExpanderGroup.GetPanel(Index: Integer): TCustomAdvSmoothExpanderButtonPanel;
begin
  Result := TCustomAdvSmoothExpanderButtonPanel(FPanels[Index]);
end;

procedure TAdvSmoothExpanderGroup.SetPanel(Index: Integer; const Value: TCustomAdvSmoothExpanderButtonPanel);
begin
  TCustomAdvSmoothExpanderButtonPanel(FPanels[Index]).Assign(Value);
end;

procedure TAdvSmoothExpanderGroup.MovePanel(FromIndex, ToIndex: Integer);
var
  i: Integer;
begin
  FPanels.Move(FromIndex,ToIndex);
  for i := 1 to FPanels.Count do
  begin
    TCustomAdvSmoothExpanderButtonPanel(FPanels[i - 1]).Index := FPanels.Count - (i - 1);
  end;
  FCode := True;
  ArrangeControls;
  FCode := False;
end;

procedure TAdvSmoothExpanderGroup.BeginUpdate;
begin
  SendMessage(Handle,WM_SETREDRAW,Integer(False),0);
  inc(FUpdateCount);
end;

procedure TAdvSmoothExpanderGroup.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      SendMessage(Handle,WM_SETREDRAW,Integer(True),0);
      UpdateScrollBar;
      ArrangeControls;
      UpdateScrollBar;
    end;
  end;
end;

procedure TAdvSmoothExpanderGroup.FillChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvSmoothExpanderGroup.SetColorTones(ATones: TColorTones);
var
  i: integer;
begin
  Fill.Color := ATones.Background.BrushColor;
  Fill.ColorTo := ATones.Background.BrushColor;
  for I := 0 to PanelCount - 1 do
    Panels[I].SetColorTones(ATones);
end;

procedure TAdvSmoothExpanderGroup.SetColumns(const Value: Integer);
begin
  if (Value >= 1) then
  begin
    FColumns := Value;
    UpdateScrollBar;
    ArrangeControls;
    UpdateScrollBar;
  end;
end;

procedure TAdvSmoothExpanderGroup.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

procedure TAdvSmoothExpanderGroup.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothExpanderGroup.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

procedure TAdvSmoothExpanderGroup.SetComponentStyle(AStyle: TTMSStyle);
var
  i: integer;
begin
  FTMSStyle := AStyle;
  Style := AStyle;
  case AStyle of
    tsOffice2003Blue:
      begin
        Fill.Color := $00FFD2AF;
        Fill.ColorTo := $00FFD2AF;
      end;
    tsOffice2003Silver:
      begin
        Fill.Color := $00E6D8D8;
        Fill.ColorTo := $00E6D8D8;
      end;
    tsOffice2003Olive:
      begin
        Fill.Color := $CFF0EA;
        Fill.ColorTo := $CFF0EA;
      end;
    tsOffice2003Classic:
      begin
        Fill.Color := $00F2F2F2;
        Fill.ColorTo := $00F2F2F2;
      end;
    tsOffice2007Luna:
      begin
        Fill.Color := $00FFD2AF;
        Fill.ColorTo := $00FFD2AF;
      end;
    tsOffice2007Obsidian:
      begin
        Fill.Color := $5C534C;
        Fill.ColorTo := $5C534C;
      end;
    tsWindowsXP:
      begin
        Fill.Color := $00B6B6B6;
        Fill.ColorTo := $00B6B6B6;
      end;
    tsWhidbey:
      begin
        Fill.Color := $F5F9FA;
        Fill.ColorTo := $F5F9FA;
      end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
        Fill.Color := $00CAC1BA;
        Fill.ColorTo := $00CAC1BA;
      end;
    tsWindowsVista:
      begin
        Fill.Color := $FDF8F1;
        Fill.ColorTo := $FCEFD5;
      end;
    tsTerminal:
      begin
        Fill.Color := clBtnFace;
        Fill.ColorTo := clBtnFace;
      end;
        tsOffice2010Blue:
      begin
        Fill.Color := $F0DAC7;
        Fill.ColorTo := $F0DAC7;
      end;
        tsOffice2010Silver:
      begin
        Fill.Color := $EDE5E0;
        Fill.ColorTo := $EDE5E0;
      end;
        tsOffice2010Black:
      begin
        Fill.Color := $BFBFBF;
        Fill.ColorTo := $BFBFBF;
      end;
      tsWindows8, tsWindows10:
      begin
        Fill.Color := $F7F6F5;
        Fill.ColorTo := $F7F6F5;
      end;
      tsOffice2013White:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;
      end;
      tsOffice2013LightGray:
      begin
        Fill.Color := $F6F6F6;
        Fill.ColorTo := $F6F6F6;
      end;
      tsOffice2013Gray:
      begin
        Fill.Color := $E5E5E5;
        Fill.ColorTo := $E5E5E5;
      end;
      tsOffice2016White:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;
      end;
      tsOffice2016Gray:
      begin
        Fill.Color := $B2B2B2;
        Fill.ColorTo := $B2B2B2;
      end;
      tsOffice2016Black:
      begin
        Fill.Color := $363636;
        Fill.ColorTo := $363636;
      end;
  end;

  for I := 0 to PanelCount - 1 do
    Panels[I].SetComponentStyle(AStyle);
end;

procedure TAdvSmoothExpanderGroup.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill.Assign(Value);
    FillChanged(Self);
  end;
end;

//procedure TAdvSmoothExpanderGroup.SetGroupStyle(const Value: TAdvSmoothExpanderGroupStyle);
//begin
//  if FGroupStyle <> Value then
//  begin
//    FGroupStyle := Value;
//    UpdateScrollBar;
//    ArrangeControls;
//  end;
//end;

function TAdvSmoothExpanderGroup.GetPanelCount: Integer;
begin
  Result := FPanels.Count;
end;

function TAdvSmoothExpanderGroup.GetThemeID: String;
begin
  Result := ClassName;
end;

procedure TAdvSmoothExpanderGroup.AlignControls(AControl: TControl;
  var Rect: TRect);
begin
  inherited;
  UpdateScrolLbar;
end;

end.

