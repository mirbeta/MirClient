unit UnboundSimpleDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls,
  Forms, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxEdit,
  cxEditRepositoryItems, cxGridTableView,  ActnList, ImgList,
  Menus, cxGridLevel, cxGridCustomTableView, cxClasses, Dialogs,
  cxControls, cxGridCustomView, cxGrid, ComCtrls, StdCtrls, cxDataStorage,
  cxLookAndFeels, cxLookAndFeelPainters, BaseForm;

type
  TUnboundSimpleDemoMainForm = class(TfmBaseForm)
    cxGrid: TcxGrid;
    lvPlanets: TcxGridLevel;
    tvPlanets: TcxGridTableView;
    tvPlanetsNAME: TcxGridColumn;
    tvPlanetsNO: TcxGridColumn;
    tvPlanetsORBITS: TcxGridColumn;
    tvPlanetsDISTANCE: TcxGridColumn;
    tvPlanetsPERIOD: TcxGridColumn;
    tvPlanetsDISCOVERER: TcxGridColumn;
    tvPlanetsDATE: TcxGridColumn;
    tvPlanetsRADIUS: TcxGridColumn;
    edrepMain: TcxEditRepository;
    edrepCenterText: TcxEditRepositoryTextItem;
    edrepRightText: TcxEditRepositoryTextItem;
    procedure FormCreate(Sender: TObject);
  private
    procedure CustomizeColumns;
    procedure LoadData;
    procedure SetFilter;
  protected
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
  end;

var
  UnboundSimpleDemoMainForm: TUnboundSimpleDemoMainForm;

implementation

{$R *.dfm}

uses
  Variants, AboutDemoForm, cxFormats;

procedure TUnboundSimpleDemoMainForm.FormCreate(Sender: TObject);
begin
  tvPlanets.BeginUpdate;
  try
    CustomizeColumns;
    LoadData;
    SetFilter;
  finally
    tvPlanets.EndUpdate;
  end;
end;

procedure TUnboundSimpleDemoMainForm.CustomizeColumns;
const
  cDistance = 3;
  cPeriod = 4;
  cRadius = 7;
var
  I: Integer;
begin
  dxFormatSettings.DecimalSeparator := '.';
  with tvPlanets do
    for I := 0 to ColumnCount - 1 do
      if I in [cDistance, cRadius] then
        Columns[I].DataBinding.ValueTypeClass := TcxIntegerValueType
      else
        if I in [cPeriod] then
          Columns[I].DataBinding.ValueTypeClass := TcxFloatValueType
        else
          Columns[I].DataBinding.ValueTypeClass := TcxStringValueType;
end;

procedure TUnboundSimpleDemoMainForm.LoadData;
const
  AFileName = 'nineplanets.txt';
  AHeaderLineCount = 2;

var
  ARecords, AValues: TStringList;
  I: Integer;

  procedure InitRecord(const Str: string);
  var
    J: Integer;
    V: Variant;
  begin
    AValues.CommaText := Str;
    for J := 0 to AValues.Count - 1 do
     if AValues.Strings[J] <> '-' then
     begin
      V := AValues.Strings[J];
      if not VarIsNull(V) then
        tvPlanets.DataController.Values[I, J] := V;
     end;
  end;

begin
  if not FileExists(AFileName) then
    raise Exception.Create('Data file not found');

  ARecords := TStringList.Create;
  try
    AValues := TStringList.Create;
    try
      tvPlanets.BeginUpdate;
      with ARecords do
      try
        LoadFromFile(AFileName);
        tvPlanets.DataController.RecordCount := Count - AHeaderLineCount;
        for I := 0 to Count - (AHeaderLineCount + 1) do
          InitRecord(Strings[I + AHeaderLineCount]);
      finally
        tvPlanets.EndUpdate;
      end;  
    finally  
      AValues.Free;
    end;
  finally  
    ARecords.Free;
  end;
end;

procedure TUnboundSimpleDemoMainForm.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  UpdateTableViewStyleSheet(tvPlanets);
end;

procedure TUnboundSimpleDemoMainForm.SetFilter;
begin
  tvPlanets.DataController.Filter.AddItem(nil, tvPlanetsORBITS, foEqual, 'Sun', 'Sun');
  tvPlanets.DataController.Filter.Active := True;
end;

end.  
