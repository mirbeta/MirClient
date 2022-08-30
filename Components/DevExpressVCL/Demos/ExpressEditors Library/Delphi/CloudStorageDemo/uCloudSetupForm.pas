unit uCloudSetupForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxContainer, cxEdit, cxTextEdit, cxMemo, cxRichEdit,
  dxLayoutcxEditAdapters, dxLayoutContainer, cxClasses,
  dxLayoutControl, dxLayoutLookAndFeels, cxCheckBox, dxLayoutControlAdapters,
  cxButtons, Menus, StdCtrls;

type
  TfmCloudSetupWizard = class(TForm)
    dxLayoutControl2: TdxLayoutControl;
    reGoogleApi: TcxRichEdit;
    teGoogleApiClientSecret: TcxTextEdit;
    teGoogleApiClientID: TcxTextEdit;
    reMSGraph: TcxRichEdit;
    teMSGraphClientID: TcxTextEdit;
    teMSGraphClientSecret: TcxTextEdit;
    btnStart: TcxButton;
    btnCancel: TcxButton;
    reAbout: TcxRichEdit;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutGroup5: TdxLayoutGroup;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    procedure teChange(Sender: TObject);
    procedure reURLClick(Sender: TcxCustomRichEdit;
      const URLText: string; Button: TMouseButton);
    procedure FormCreate(Sender: TObject);
  private
    procedure UpdateStateButtons;
  public
    { Public declarations }
  end;

implementation

uses
  dxCore;

{$R *.dfm}

procedure TfmCloudSetupWizard.FormCreate(Sender: TObject);
begin
  reAbout.Lines.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Wizard.rtf');
  reMSGraph.Lines.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Wizard-MSGraph.rtf');
  reGoogleApi.Lines.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Wizard-GoogleAPI.rtf');
end;

procedure TfmCloudSetupWizard.reURLClick(
  Sender: TcxCustomRichEdit; const URLText: string; Button: TMouseButton);
begin
  dxShellExecute(Sender.Handle, URLText);
end;

procedure TfmCloudSetupWizard.teChange(
  Sender: TObject);
begin
  UpdateStateButtons;
end;

procedure TfmCloudSetupWizard.UpdateStateButtons;
begin
  btnStart.Enabled := ((Trim(teMSGraphClientID.Text) <> '') and (Trim(teMSGraphClientSecret.Text) <> '')) or
    ((Trim(teGoogleApiClientID.Text) <> '') and (Trim(teGoogleApiClientSecret.Text) <> ''))
end;

end.
