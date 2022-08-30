unit DemoBasicMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Controls, Forms, cxStyles, Graphics,
  cxCustomData, cxGraphics, cxFilter, cxData, cxEdit, ActnList, ImgList, Menus,
  cxClasses, Dialogs, cxControls, ComCtrls, StdCtrls, dxPSGlbl, dxPSUtl, dxPSEngn,
  dxPrnPg, dxBkgnd, dxWrap, dxPrnDev, dxPSCompsProvider, dxPSFillPatterns,
  dxPSEdgePatterns, dxPSCore, cxDataStorage, ToolWin, cxTextEdit,
  dxPScxCommon, cxLookAndFeels, cxEditRepositoryItems,
  cxLookAndFeelPainters, dxPSPDFExportCore, dxPSPDFExport, cxDrawTextUtils,
  dxPSPrVwStd, dxPScxEditorProducers, dxPScxExtEditorProducers,
  dxPScxPageControlProducer;

type
  TDemoBasicMainForm = class(TForm)
    mmMain: TMainMenu;
    miKind: TMenuItem;
    miFlat: TMenuItem;
    miStandard: TMenuItem;
    miUltraFlat: TMenuItem;
    miOffice11: TMenuItem;
    miNativeStyle: TMenuItem;
    miHelp: TMenuItem;
    miDeveloperExpressontheweb: TMenuItem;
    ToolButton1: TToolButton;
    ilMain: TcxImageList;
    sty: TActionList;
    actDXOnTheWeb: TAction;
    miFile: TMenuItem;
    miOptions: TMenuItem;
    miExit: TMenuItem;
    actExit: TAction;
    sbMain: TStatusBar;
    actShowDemoDescription: TAction;
    miShowDemoDescription: TMenuItem;
    miSeparator2: TMenuItem;
    lbDescrip: TLabel;
    dxComponentPrinter: TdxComponentPrinter;
    ToolBar1: TToolBar;
    tbtnPageSetup: TToolButton;
    tbtnPreview: TToolButton;
    tbtnPrint: TToolButton;
    tbtnDesigner: TToolButton;
    miPirntingOptions: TMenuItem;
    N2: TMenuItem;
    Print1: TMenuItem;
    miPrintPreview: TMenuItem;
    miPageSetup: TMenuItem;
    actPageSetup: TAction;
    actPreview: TAction;
    actPrint: TAction;
    actDesigner: TAction;
    dxPSEngineController1: TdxPSEngineController;
    miPreviewDialogStyles: TMenuItem;
    miDialogsLookAndFeel: TMenuItem;
    actAbout: TAction;
    N10: TMenuItem;
    Aboutthisdemo1: TMenuItem;
    actImageAntiAliasing: TAction;
    tlbtnAntiAliasing: TToolButton;
    miAntiAliasing: TMenuItem;
    procedure AlwaysEnabled(Sender: TObject);
    procedure actDXOnTheWebExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actShowDemoDescriptionExecute(Sender: TObject);
    procedure actPageSetupExecute(Sender: TObject);
    procedure actPreviewExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actDesignerExecute(Sender: TObject);
    procedure DialogsLookAndFeelChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actImageAntiAliasingExecute(Sender: TObject);
  public
    procedure PreviewDialogStyleClick(Sender: TObject);
  end;

var
  DemoBasicMainForm: TDemoBasicMainForm;

implementation

uses
  ShellApi, AboutDemoForm;

{$R *.dfm}

procedure TDemoBasicMainForm.AlwaysEnabled(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := True;
end;

procedure TDemoBasicMainForm.actDXOnTheWebExecute(
  Sender: TObject);
begin
  ShellExecute(Handle, PChar('OPEN'), PChar('http://www.devexpress.com'), nil, nil, SW_SHOWMAXIMIZED);
end;

procedure TDemoBasicMainForm.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TDemoBasicMainForm.actShowDemoDescriptionExecute(
  Sender: TObject);
begin
  lbDescrip.Visible := not lbDescrip.Visible;
  TCustomAction(Sender).Checked := lbDescrip.Visible;
end;

procedure TDemoBasicMainForm.actPageSetupExecute(Sender: TObject);
begin
  dxComponentPrinter.PageSetup(nil);
end;

procedure TDemoBasicMainForm.actPreviewExecute(Sender: TObject);
begin
  dxComponentPrinter.Preview(True, nil);
end;

procedure TDemoBasicMainForm.actPrintExecute(Sender: TObject);
begin
  dxComponentPrinter.Print(True, nil, nil);
end;

procedure TDemoBasicMainForm.actDesignerExecute(Sender: TObject);
begin
  dxComponentPrinter.DesignReport(nil);
end;

procedure TDemoBasicMainForm.DialogsLookAndFeelChanged(Sender: TObject);
begin
  if TMenuItem(Sender).Tag < 4 then
  begin
    TMenuItem(Sender).Checked := True;
    dxPSEngineController1.DialogsLookAndFeel.Kind := TcxLookAndFeelKind(TMenuItem(Sender).Tag);
    miNativeStyle.Checked := False;
  end;
  dxPSEngineController1.DialogsLookAndFeel.NativeStyle := miNativeStyle.Checked;
end;

procedure TDemoBasicMainForm.FormCreate(Sender: TObject);

  function CreateStyleItem(AOwner: TComponent; ACaption: string; ATag: Integer): TMenuItem;
  begin
    Result := TMenuItem.Create(AOwner);
    Result.Caption := ACaption;
    Result.RadioItem := True;
    Result.GroupIndex := 2;
    Result.Tag := ATag;
    Result.OnClick := PreviewDialogStyleClick;
    if dxPSEngineController1.PreviewDialogStyle = ACaption then
      Result.Checked := True;
  end;

var
  I: Integer;
begin
  inherited;
  miAntiAliasing.ImageIndex := -1;
  FSmoothlyStretchImages := actImageAntiAliasing.Checked;
  miPreviewDialogStyles.Clear;
  for I := 0 to dxPSPreviewDialogManager.Count - 1 do
    miPreviewDialogStyles.Add(CreateStyleItem(miPreviewDialogStyles,
      dxPSPreviewDialogManager.Names[I], I));
  miNativeStyle.Checked := dxPSEngineController1.DialogsLookAndFeel.NativeStyle;
  for I := 0 to miKind.Count - 1 do
  begin
    miKind.Items[I].Checked :=
      Ord(dxPSEngineController1.DialogsLookAndFeel.Kind) = miKind.Items[I].Tag;
  end;
end;

procedure TDemoBasicMainForm.PreviewDialogStyleClick(Sender: TObject);
begin
  dxPSEngineController1.PreviewDialogStyle :=
    dxPSPreviewDialogManager.Names[TMenuItem(Sender).Tag];
  TMenuItem(Sender).Checked := True;
end;

procedure TDemoBasicMainForm.actAboutExecute(Sender: TObject);
begin
  ShowAboutDemoForm;
end;

procedure TDemoBasicMainForm.actImageAntiAliasingExecute(Sender: TObject);
begin
  FSmoothlyStretchImages := actImageAntiAliasing.Checked;
end;

end.
