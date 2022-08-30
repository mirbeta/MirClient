unit LayoutControlRLMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DemoBasicMain, dxPSGlbl, dxPSUtl, dxPSEngn, dxPrnPg, dxBkgnd,
  dxWrap, dxPrnDev, dxPSCompsProvider, dxPSFillPatterns, dxPSEdgePatterns,
  dxPSPDFExportCore, dxPSPDFExport, cxDrawTextUtils, dxPSPrVwStd,
  dxPScxEditorProducers, dxPScxExtEditorProducers,
  dxPScxPageControlProducer, ImgList, cxGraphics, dxPSCore, ActnList,
  Menus, ComCtrls, ToolWin, StdCtrls, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, dxLayoutcxEditAdapters,
  dxLayoutControl, cxNavigator, cxDBNavigator, cxMemo, cxDBEdit, cxImage,
  cxHyperLinkEdit, cxCheckBox, cxCurrencyEdit, cxCalc, cxDropDownEdit,
  cxSpinEdit, cxTimeEdit, cxTextEdit, cxMaskEdit, cxCalendar,
  dxPScxDBEditorLnks, dxPSTextLnk, dxPSContainerLnk, dxPSdxLCLnk,
  dxLayoutLookAndFeels, dxLayoutContainer;

type
  TLayoutControlMainForm = class(TDemoBasicMainForm)
    cxImageList1: TcxImageList;
    Customization1: TMenuItem;
    N1: TMenuItem;
    lcMain: TdxLayoutControl;
    cxDBDateEdit1: TcxDBDateEdit;
    cxDBTimeEdit1: TcxDBTimeEdit;
    cxDBComboBox1: TcxDBComboBox;
    cxDBCalcEdit1: TcxDBCalcEdit;
    cxDBCurrencyEdit1: TcxDBCurrencyEdit;
    cxDBTextEdit1: TcxDBTextEdit;
    cxDBTextEdit2: TcxDBTextEdit;
    cxDBTextEdit3: TcxDBTextEdit;
    cxDBTextEdit4: TcxDBTextEdit;
    cxDBTextEdit5: TcxDBTextEdit;
    cxDBCheckBox1: TcxDBCheckBox;
    cxDBTextEdit6: TcxDBTextEdit;
    cxDBTextEdit7: TcxDBTextEdit;
    cxDBTextEdit8: TcxDBTextEdit;
    cxDBTextEdit9: TcxDBTextEdit;
    cxDBMaskEdit1: TcxDBMaskEdit;
    cxDBTextEdit10: TcxDBTextEdit;
    cxDBMaskEdit2: TcxDBMaskEdit;
    cxDBMaskEdit3: TcxDBMaskEdit;
    cxDBHyperLinkEdit1: TcxDBHyperLinkEdit;
    cxDBTextEdit12: TcxDBTextEdit;
    cxDBTextEdit11: TcxDBTextEdit;
    cxDBHyperLinkEdit2: TcxDBHyperLinkEdit;
    cxDBCurrencyEdit2: TcxDBCurrencyEdit;
    cxDBSpinEdit1: TcxDBSpinEdit;
    cxDBSpinEdit2: TcxDBSpinEdit;
    cxDBSpinEdit3: TcxDBSpinEdit;
    cxDBSpinEdit4: TcxDBSpinEdit;
    cxDBSpinEdit5: TcxDBSpinEdit;
    cxDBSpinEdit6: TcxDBSpinEdit;
    cxDBCheckBox2: TcxDBCheckBox;
    cxDBImage1: TcxDBImage;
    cxDBMemo1: TcxDBMemo;
    cxDBNavigator1: TcxDBNavigator;
    lcMainGroup_Root1: TdxLayoutGroup;
    lcMainGroup2: TdxLayoutGroup;
    lcMainGroup7: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    lcMainItem2: TdxLayoutItem;
    lcMainGroup8: TdxLayoutGroup;
    lcMainItem3: TdxLayoutItem;
    lcMainItem24: TdxLayoutItem;
    lcMainItem4: TdxLayoutItem;
    lcMainItem5: TdxLayoutItem;
    dxLayoutGroup1: TdxLayoutGroup;
    lcMainGroup18: TdxLayoutGroup;
    lcMainGroup17: TdxLayoutGroup;
    lcMainItem8: TdxLayoutItem;
    lcMainItem6: TdxLayoutItem;
    lcMainItem7: TdxLayoutItem;
    lcMainSeparatorItem2: TdxLayoutSeparatorItem;
    lcMainItem9: TdxLayoutItem;
    dxLayoutSplitterItem1: TdxLayoutSplitterItem;
    lcMainGroup10: TdxLayoutGroup;
    lcMainItem14: TdxLayoutItem;
    lcMainItem15: TdxLayoutItem;
    lcMainItem16: TdxLayoutItem;
    lcMainSplitterItem2: TdxLayoutSplitterItem;
    lcMainGroup16: TdxLayoutGroup;
    lcMainItem12: TdxLayoutItem;
    lcMainItem10: TdxLayoutItem;
    lcMainItem13: TdxLayoutItem;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    lcMainItem17: TdxLayoutItem;
    lcMainGroup13: TdxLayoutGroup;
    lcMainItem18: TdxLayoutItem;
    lcMainItem19: TdxLayoutItem;
    lcMainItem20: TdxLayoutItem;
    lcMainGroup3: TdxLayoutGroup;
    lcMainGroup4: TdxLayoutGroup;
    lcMainItem21: TdxLayoutItem;
    lcMainItem22: TdxLayoutItem;
    lcMainItem23: TdxLayoutItem;
    lcMainGroup5: TdxLayoutGroup;
    lcMainGroup14: TdxLayoutGroup;
    lcMainItem25: TdxLayoutItem;
    lcMainItem26: TdxLayoutItem;
    lcMainItem27: TdxLayoutItem;
    lcMainGroup15: TdxLayoutGroup;
    lcMainItem31: TdxLayoutItem;
    lcMainItem30: TdxLayoutItem;
    lcMainGroup6: TdxLayoutGroup;
    lcMainItem32: TdxLayoutItem;
    lcMainItem33: TdxLayoutItem;
    lcMainItem34: TdxLayoutItem;
    lcMainItem11: TdxLayoutItem;
    lcMainItem28: TdxLayoutItem;
    lcMainItem29: TdxLayoutItem;
    dxComponentPrinterLink1: TdxLayoutControlReportLink;
    miStyle: TMenuItem;
    miUsecxLookAndFeel: TMenuItem;
    miUsecxLookAndFeelNative: TMenuItem;
    miUsecxLookAndFeelOffice11: TMenuItem;
    miUsecxLookAndFeelStandard: TMenuItem;
    miUsecxLookAndFeelFlat: TMenuItem;
    miUsecxLookAndFeelUltraFlat: TMenuItem;
    miLayoutWeb: TMenuItem;
    miLayoutOffice: TMenuItem;
    miLayoutStandard: TMenuItem;
    acLayoutOffice: TAction;
    acLayoutWeb: TAction;
    acFlat: TAction;
    acStandard: TAction;
    acUltraFlat: TAction;
    acOffice11: TAction;
    acNative: TAction;
    acLayoutStandard: TAction;
    llcfMain: TdxLayoutLookAndFeelList;
    dxLayoutStandardLookAndFeel1: TdxLayoutStandardLookAndFeel;
    dxLayoutOfficeLookAndFeel1: TdxLayoutOfficeLookAndFeel;
    dxLayoutWebLookAndFeel1: TdxLayoutWebLookAndFeel;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutSkinLookAndFeel1: TdxLayoutSkinLookAndFeel;
    procedure Customization1Click(Sender: TObject);
    procedure dxLayoutGroup1Button0Click(Sender: TObject);
    procedure acLayoutStyleExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LayoutControlMainForm: TLayoutControlMainForm;

implementation

uses DemoDM;

{$R *.dfm}

procedure TLayoutControlMainForm.Customization1Click(Sender: TObject);
begin
  lcMain.Customization := True;
end;

procedure TLayoutControlMainForm.dxLayoutGroup1Button0Click(
  Sender: TObject);
begin
  dxLayoutGroup1.Parent := nil;
end;

procedure TLayoutControlMainForm.acLayoutStyleExecute(Sender: TObject);
var
  ATag: Integer;
begin
  ATag := (Sender as TAction).Tag;
  case ATag of
    0:
      lcMain.LayoutLookAndFeel := dxLayoutStandardLookAndFeel1;
    1:
      lcMain.LayoutLookAndFeel := dxLayoutOfficeLookAndFeel1;
    2:
      lcMain.LayoutLookAndFeel := dxLayoutWebLookAndFeel1;
  else
    lcMain.LayoutLookAndFeel := dxLayoutCxLookAndFeel1;
    case ATag of
      3..6:
        begin
          dxLayoutCxLookAndFeel1.LookAndFeel.NativeStyle := False;
          dxLayoutCxLookAndFeel1.LookAndFeel.Kind := TcxLookAndFeelKind(ATag - 3);
        end;
      7:
        dxLayoutCxLookAndFeel1.LookAndFeel.NativeStyle := True;
    end;
  end;
end;

procedure TLayoutControlMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  acLayoutStandard.Execute;
end;

end.
