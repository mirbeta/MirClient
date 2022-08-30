unit OLAPBrowserMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DemoBasicMain, cxLookAndFeels, Menus, StdCtrls, cxControls,
  cxCustomPivotGrid, cxPivotGrid, cxCustomData, cxClasses, cxGraphics,
  cxStyles, cxPivotGridOLAPConnectionDesigner, cxPivotGridOLAPDataSource,
  cxEdit, cxLookAndFeelPainters, ExtCtrls, cxSplitter;

type
  TfrmOlapBrowser = class(TfrmDemoBasicMain)
    UnboundPivotGrid: TcxPivotGrid;
    NewConnection1: TMenuItem;
    N3: TMenuItem;
    OLAPDataSource: TcxPivotGridOLAPDataSource;
    Panel1: TPanel;
    cxSplitter1: TcxSplitter;
    miADOMDProvider: TMenuItem;
    miLockedStateImage: TMenuItem;
    miLockedStateImageEffect: TMenuItem;
    miLockedStateImageEffectDark: TMenuItem;
    miLockedStateImageEffectLight: TMenuItem;
    miLockedStateImageMode: TMenuItem;
    miLockedStateImageModeImmediate: TMenuItem;
    miLockedStateImageModeNever: TMenuItem;
    miLockedStateImageModePending: TMenuItem;
    miOLEDBProvider: TMenuItem;
    miProvider: TMenuItem;
    procedure ChangeProviderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LockedViewImageClick(Sender: TObject);
    procedure NewConnection1Click(Sender: TObject);
  private
    procedure LoadDefaultLayout(AActivate: Boolean);
  protected
    function GetPivotGrid: TcxCustomPivotGrid; override;
    procedure SetFieldPos(const AFieldName: string; AArea: TcxPivotGridFieldArea);
    procedure SyncMenuWithOptionsLockedStateImage;
  public
    procedure AfterConstruction; override;
  end;

var
  frmOlapBrowser: TfrmOlapBrowser;

implementation

{$R *.dfm}

uses DemoUtils, cxPivotGridOLAPADOMDProvider, cxPivotGridOLAPOLEDBProvider;

procedure TfrmOlapBrowser.ChangeProviderClick(Sender: TObject);
var
  AActive: Boolean;
begin
  AActive := OLAPDataSource.Active;
  dxDemoMenuItemSetChecked(Sender, True);
  if dxDemoFindMenuItem(mmMain, 'miADOMDProvider') = Sender then
    OLAPDataSource.ProviderClass := TcxPivotGridOLAPADOMDProvider
  else
    OLAPDataSource.ProviderClass := TcxPivotGridOLAPOLEDBProvider;
  if not OLAPDataSource.Active then
    LoadDefaultLayout(AActive);
end;

procedure TfrmOlapBrowser.AfterConstruction;
begin
  inherited;
  SyncMenuWithOptionsLockedStateImage;
end;

function TfrmOlapBrowser.GetPivotGrid: TcxCustomPivotGrid;
begin
  Result := UnboundPivotGrid;
end;

procedure TfrmOlapBrowser.SetFieldPos(
  const AFieldName: string; AArea: TcxPivotGridFieldArea);
var
  AField: TcxPivotGridField;
begin
  AField := PivotGrid.GetFieldByName(AFieldName);
  if AField = nil then Exit;
  AField.Area := AArea;
  AField.Visible := True;
end;

procedure TfrmOlapBrowser.FormCreate(Sender: TObject);
var
  APath, ConnectionString: string;
begin
  inherited;
  APath := ExpandFileName('..\..\Data\Northwind.cub');
  if not FileExists(APath) then Exit;
  ConnectionString := 'Provider=MSOLAP;Integrated Security=SSPI;Persist Security Info=False;Data Source=';
  OLAPDataSource.ConnectionString := ConnectionString + APath;
  LoadDefaultLayout(True);
  PivotGrid.Customization.Visible := True;
end;

procedure TfrmOlapBrowser.NewConnection1Click(Sender: TObject);
var
  I, J: Integer;
  ACube, ANewConnectionString: string;
begin
  ANewConnectionString := cxPivotGridOLAPCreateConnectionString(ACube, OLAPDataSource.ProviderClass,
    PivotGrid.LookAndFeel);
  if ANewConnectionString <> '' then
  begin
    PivotGrid.BeginUpdate;
    try
      OLAPDataSource.ConnectionString := ANewConnectionString;
      OLAPDataSource.Cube := ACube;
      OLAPDataSource.Active := True;
      OLAPDataSource.RetrieveFields(PivotGrid);
      for I := 0 to PivotGrid.Groups.Count - 1 do
        for J := 1 to PivotGrid.Groups[I].FieldCount - 1 do
          PivotGrid.Groups[I].Fields[J].Visible := True;
    finally
      PivotGrid.EndUpdate;
      PivotGrid.Customization.Visible := True;
    end;
  end;
end;

procedure TfrmOlapBrowser.SyncMenuWithOptionsLockedStateImage;
var
  AOptionsLockedStateImage: TcxPivotGridOptionsLockedStateImage;
begin
  AOptionsLockedStateImage := PivotGrid.OptionsLockedStateImage;
  dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miLockedStateImageModeImmediate'),
    AOptionsLockedStateImage.Mode = lsimImmediate);
  dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miLockedStateImageModeNever'),
    AOptionsLockedStateImage.Mode = lsimNever);
  dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miLockedStateImageModePending'),
    AOptionsLockedStateImage.Mode = lsimPending);
  dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miLockedStateImageEffectLight'),
    AOptionsLockedStateImage.Effect = lsieLight);
  dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miLockedStateImageEffectDark'),
    AOptionsLockedStateImage.Effect = lsieDark);
end;

procedure TfrmOlapBrowser.LockedViewImageClick(Sender: TObject);
var
  ATag: Integer;
begin
  ATag := TComponent(Sender).Tag;
  case ATag of
    0, 1, 2:
      PivotGrid.OptionsLockedStateImage.Mode := TcxLockedStateImageShowingMode(ATag);
    3, 4:
      PivotGrid.OptionsLockedStateImage.Effect := TcxLockedStateImageEffect(ATag - 2);
  end;
  SyncMenuWithOptionsLockedStateImage;
end;

procedure TfrmOlapBrowser.LoadDefaultLayout(AActivate: Boolean);
begin
  OLAPDataSource.Active := AActivate;
  PivotGrid.BeginUpdate;
  try
    if AActivate then
    begin
      OLAPDataSource.RetrieveFields(PivotGrid);
      SetFieldPos('Country', faColumn);
      SetFieldPos('City', faColumn);
      SetFieldPos('Category Name', faRow);
      SetFieldPos('Products', faRow);
      SetFieldPos('Quantity', faData);
      SetFieldPos('Discount', faData);
    end;
  finally
    PivotGrid.EndUpdate;
    PivotGrid.ApplyBestFit;
  end;
end;

end.
