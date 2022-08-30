unit WebServiceDemoSetupForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxContainer, cxEdit, cxTextEdit, cxMemo, cxRichEdit, dxCustomWizardControl,
  dxWizardControl, dxLayoutcxEditAdapters, dxLayoutContainer, cxClasses,
  dxLayoutControl, dxLayoutLookAndFeels, cxCheckBox, dxLayoutControlAdapters,
  cxButtons, Menus, StdCtrls;

type
  TWebServiceDemoSetupWizard = class(TForm)
    dxLayoutControl2: TdxLayoutControl;
    reGoogleApi: TcxRichEdit;
    teGoogleApiClientSecret: TcxTextEdit;
    teGoogleApiClientID: TcxTextEdit;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    reMSGraph: TcxRichEdit;
    dxLayoutItem2: TdxLayoutItem;
    teMSGraphClientID: TcxTextEdit;
    dxLayoutItem3: TdxLayoutItem;
    teMSGraphClientSecret: TcxTextEdit;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutGroup5: TdxLayoutGroup;
    btnStart: TcxButton;
    dxLayoutItem7: TdxLayoutItem;
    btnCancel: TcxButton;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    reAbout: TcxRichEdit;
    procedure teChange(Sender: TObject);
    procedure reURLClick(Sender: TcxCustomRichEdit;
      const URLText: string; Button: TMouseButton);
    procedure FormCreate(Sender: TObject);
  private
    procedure UpdateStateButtons;
  public
    { Public declarations }
  end;

var
  WebServiceDemoSetupWizard: TWebServiceDemoSetupWizard;

implementation

uses
  dxCore;

{$R *.dfm}

procedure TWebServiceDemoSetupWizard.FormCreate(Sender: TObject);
begin
  reAbout.Lines.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Wizard.rtf');
  reMSGraph.Lines.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Wizard-MSGraph.rtf');
  reGoogleApi.Lines.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Wizard-GoogleAPI.rtf');
end;

procedure TWebServiceDemoSetupWizard.reURLClick(
  Sender: TcxCustomRichEdit; const URLText: string; Button: TMouseButton);
begin
  dxShellExecute(Sender.Handle, URLText);
end;

procedure TWebServiceDemoSetupWizard.teChange(
  Sender: TObject);
begin
  UpdateStateButtons;
end;

procedure TWebServiceDemoSetupWizard.UpdateStateButtons;
begin
  btnStart.Enabled := ((Trim(teMSGraphClientID.Text) <> '') and (Trim(teMSGraphClientSecret.Text) <> '')) or
    ((Trim(teGoogleApiClientID.Text) <> '') and (Trim(teGoogleApiClientSecret.Text) <> ''))
end;

end.
