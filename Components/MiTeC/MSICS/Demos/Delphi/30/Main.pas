unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MSI_Monitor, ExtCtrls, StdCtrls, MiTeC_MultiMon;

type
  TwndMain = class(TForm)
    scb: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FMon: TMiTeC_Monitor;
    procedure SetMonitors;
  protected
    procedure WMDISPLAYCHANGE(var Message: TWMDisplayChange); message WM_DISPLAYCHANGE;
    procedure WMDPICHANGED(var Message: TWMDpi); message WM_DPICHANGED;
  public
    procedure RefreshData;
  end;

var
  wndMain: TwndMain;

implementation

uses MiTeC_Helpers;

{$R *.dfm}

{ TwndMain }

procedure TwndMain.SetMonitors;
var
  i,x,y: integer;
  z: double;
  m: TMonitorRecord;
  p: TPanel;
  l: TLabel;
  s: string;
begin
  while scb.ControlCount>0 do
    scb.Controls[0].Free;

  z:=7*96/GetPixelsPerInch(Screen.MonitorFromWindow(Self.Handle,mdNearest).Handle);
  x:=0;
  y:=0;
  for i:=0 to FMon.Count-1 do
    if Round(RectHeight(FMon.Monitors[i].Bounds)/z)>y then
      y:=Round(RectHeight(FMon.Monitors[i].Bounds)/z);
  inc(y,10);
  for i:=0 to FMon.Count-1 do begin
    m:=FMon.Monitors[i];

    p:=TPanel.Create(Self);
    p.BevelOuter:=bvNone;
    p.BevelKind:=bkFlat;
    p.Font.Size:=20;
    p.Font.Color:=clGray;
    p.Font.Style:=[fsBold];
    p.ParentBackground:=False;
    if m.Primary then
      p.Color:=clCream;

    p.Parent:=scb;
    p.Width:=Round(RectWidth(m.Bounds)/z);
    p.Height:=Round(RectHeight(m.Bounds)/z);
    p.Top:=y-p.Height;
    p.Left:=x;
    p.Caption:=IntToStr(m.IdentityNum+1);

    l:=TLabel.Create(Self);
    l.AlignWithMargins:=True;
    l.Alignment:=taCenter;
    l.WordWrap:=True;
    l.ParentFont:=False;
    l.Parent:=p;
    l.Align:=alTop;
    {$IFDEF DEBUG}
    l.Caption:=Format('%s - %d'#13#10'%s'#13#10'(%d x %d) cm',[m.DeviceID,m.MonitorNum,m.DeviceDescription,m.EDID.Width,m.EDID.Height]);
    {$ELSE}
    l.Caption:=Format('%s'#13#10'(%d x %d) cm',[m.DeviceDescription,m.EDID.Width,m.EDID.Height]);
    {$ENDIF}

    l:=TLabel.Create(Self);
    l.AlignWithMargins:=True;
    l.Alignment:=taCenter;
    l.WordWrap:=True;
    l.ParentFont:=False;
    l.Parent:=p;
    l.Align:=alBottom;
    s:=m.EDID.Name;
    if s='' then
      s:=m.Model;
    l.Caption:=Format('%s'#13#10'(%d x %d) px - %d dpi',[s,RectWidth(m.Bounds),RectHeight(m.Bounds),m.DPI]);

    x:=x+p.Width+3;
  end;

end;

procedure TwndMain.FormCreate(Sender: TObject);
begin
  FMon:=TMiTeC_Monitor.Create(Self);
end;

procedure TwndMain.FormShow(Sender: TObject);
begin
  RefreshData;
end;

procedure TwndMain.RefreshData;
begin
  Screen.Cursor:=crHourglass;
  try
    FMon.RefreshData;
    SetMonitors;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TwndMain.WMDISPLAYCHANGE(var Message: TWMDisplayChange);
begin
  RefreshData;
end;

procedure TwndMain.WMDPICHANGED(var Message: TWMDpi);
begin
  RefreshData;
end;

end.
