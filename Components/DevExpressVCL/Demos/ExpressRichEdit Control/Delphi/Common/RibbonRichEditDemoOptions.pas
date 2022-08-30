unit RibbonRichEditDemoOptions;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cxGraphics, cxControls, cxLookAndFeels, ExtCtrls,
  dxRibbonSkins, cxLookAndFeelPainters, cxContainer, cxEdit, cxLabel, Menus,
  cxButtons, dxBevel, cxTextEdit, cxMaskEdit, cxDropDownEdit, dxLayoutControlAdapters, dxLayoutcxEditAdapters,
  dxLayoutContainer, cxClasses, dxLayoutControl, dxForms;

type
  TRibbonDemoStyle = (rdsOffice2007, rdsOffice2010, rdsOffice2013,
    rdsOffice2016, rdsOffice2016Tablet, rdsOffice2019, rdsScenic);

  TScreenTipOptions = record
    ShowScreenTips: Boolean;
    ShowDescripitons: Boolean;
  end;

  { TRibbonDemoOptionsForm }

  TRibbonDemoOptionsForm = class(TdxForm)
    Button1: TcxButton;
    Button2: TcxButton;
    cbColorScheme: TcxComboBox;
    cbColorSchemeAccent: TcxComboBox;
    cbRibbonStyle: TcxComboBox;
    cbScreenTipStyle: TcxComboBox;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutControl1: TdxLayoutControl;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutGroup3: TdxLayoutGroup;
    liColorSchemeAccent: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    procedure cbRibbonStyleSelect(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;

    procedure LoadOptions(const AColorSchemeName: string; const AScreenTipOptions: TScreenTipOptions;
      AStyle: TRibbonDemoStyle; AColorSchemeAccent: TdxRibbonColorSchemeAccent);
    procedure SaveOptions(out AColorSchemeName: string; out AScreenTipOptions: TScreenTipOptions;
      out AStyle: TRibbonDemoStyle; out AColorSchemeAccent: TdxRibbonColorSchemeAccent);

    class procedure PopulateColorSchemeAccents(AItems: TStrings);
    class procedure PopulateColorSchemes(AItems: TStrings; ARibbonStyle: TdxRibbonStyle);
    class procedure PopulateRibbonStyles(AItems: TStrings);
  end;

var
  RibbonDemoStyleToRibbonStyle: array[TRibbonDemoStyle] of TdxRibbonStyle = (
    rs2007, rs2010, rs2013, rs2016, rs2016Tablet, rs2019, rs2010{Scenic}
  );

function ExecuteRibbonDemoOptions(var AColorSchemeName: string; var AScreenTipOptions: TScreenTipOptions;
  var AStyle: TRibbonDemoStyle; var AColorSchemeAccent: TdxRibbonColorSchemeAccent): Boolean;
implementation

uses
  Math;

{$R *.dfm}

function ExecuteRibbonDemoOptions(var AColorSchemeName: string; var AScreenTipOptions: TScreenTipOptions;
  var AStyle: TRibbonDemoStyle; var AColorSchemeAccent: TdxRibbonColorSchemeAccent): Boolean;
begin
  with TRibbonDemoOptionsForm.Create(nil) do
  try
    LoadOptions(AColorSchemeName, AScreenTipOptions, AStyle, AColorSchemeAccent);
    Result := ShowModal = mrOk;
    if Result then
      SaveOptions(AColorSchemeName, AScreenTipOptions, AStyle, AColorSchemeAccent);
  finally
    Free;
  end;
end;

{ TRibbonDemoOptionsForm }

constructor TRibbonDemoOptionsForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PopulateRibbonStyles(cbRibbonStyle.Properties.Items);
  PopulateColorSchemeAccents(cbColorSchemeAccent.Properties.Items);
end;

procedure TRibbonDemoOptionsForm.LoadOptions(const AColorSchemeName: string;
  const AScreenTipOptions: TScreenTipOptions; AStyle: TRibbonDemoStyle;
  AColorSchemeAccent: TdxRibbonColorSchemeAccent);
begin
  cbRibbonStyle.ItemIndex := Ord(AStyle);
  cbRibbonStyleSelect(nil);
  cbColorScheme.ItemIndex := Max(0, cbColorScheme.Properties.Items.IndexOf(AColorSchemeName));
  cbColorSchemeAccent.ItemIndex := Ord(AColorSchemeAccent);
  if AScreenTipOptions.ShowScreenTips then
    cbScreenTipStyle.ItemIndex := Ord(not AScreenTipOptions.ShowDescripitons)
  else
    cbScreenTipStyle.ItemIndex := 2;
end;

procedure TRibbonDemoOptionsForm.SaveOptions(out AColorSchemeName: string;
  out AScreenTipOptions: TScreenTipOptions; out AStyle: TRibbonDemoStyle;
  out AColorSchemeAccent: TdxRibbonColorSchemeAccent);
begin
  AColorSchemeName := cbColorScheme.Text;
  AStyle := TRibbonDemoStyle(cbRibbonStyle.ItemIndex);
  AScreenTipOptions.ShowScreenTips := cbScreenTipStyle.ItemIndex <> 2;
  AScreenTipOptions.ShowDescripitons := cbScreenTipStyle.ItemIndex = 0;
  AColorSchemeAccent := TdxRibbonColorSchemeAccent(cbColorSchemeAccent.ItemIndex);
end;

class procedure TRibbonDemoOptionsForm.PopulateColorSchemeAccents(AItems: TStrings);
var
  AAccentIndex: TdxRibbonColorSchemeAccent;
begin
  AItems.BeginUpdate;
  try
    AItems.Clear;
    for AAccentIndex := Low(TdxRibbonColorSchemeAccent) to High(TdxRibbonColorSchemeAccent) do
      AItems.Add(dxRibbonColorSchemeAccentNames[AAccentIndex]);
  finally
    AItems.EndUpdate;
  end;
end;

class procedure TRibbonDemoOptionsForm.PopulateColorSchemes(AItems: TStrings; ARibbonStyle: TdxRibbonStyle);

  procedure AddColorScheme(ASkin: TdxCustomRibbonSkin);
  begin
    if ASkin.Style = ARibbonStyle then
    begin
      if AItems.IndexOf(ASkin.Name) < 0 then
        AItems.Add(ASkin.Name);
    end;
  end;

var
  I: Integer;
begin
  AItems.BeginUpdate;
  try
    AItems.Clear;
    for I := 0 to dxRibbonSkinsManager.SkinCount - 1 do
      AddColorScheme(dxRibbonSkinsManager.Skins[I]);
  finally
    AItems.EndUpdate;
  end;
end;

class procedure TRibbonDemoOptionsForm.PopulateRibbonStyles(AItems: TStrings);
const
  NamesMap: array[TRibbonDemoStyle] of string = (
    'Office 2007', 'Office 2010', 'Office 2013', 'Office 2016', 'Office 2016 Tablet', 'Office 2019', 'Scenic'
  );
var
  I: TRibbonDemoStyle;
begin
  AItems.BeginUpdate;
  try
    AItems.Clear;
    for I := Low(TRibbonDemoStyle) to High(TRibbonDemoStyle) do
      AItems.Add(NamesMap[I])
  finally
    AItems.EndUpdate;
  end;
end;

procedure TRibbonDemoOptionsForm.cbRibbonStyleSelect(Sender: TObject);
var
  ASelectedColorSchemeName: string;
begin
  liColorSchemeAccent.Enabled := cbRibbonStyle.ItemIndex > 0;

  ASelectedColorSchemeName := cbColorScheme.Text;
  PopulateColorSchemes(cbColorScheme.Properties.Items, RibbonDemoStyleToRibbonStyle[TRibbonDemoStyle(cbRibbonStyle.ItemIndex)]);
  cbColorScheme.ItemIndex := Max(0, cbColorScheme.Properties.Items.IndexOf(ASelectedColorSchemeName));
end;

end.

