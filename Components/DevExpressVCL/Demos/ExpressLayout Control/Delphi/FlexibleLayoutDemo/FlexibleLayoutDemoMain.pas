unit FlexibleLayoutDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxClasses, DB,
  dxLayoutContainer, dxLayoutControl, dxLayoutControlAdapters, dxLayoutcxEditAdapters,
  DemoDM, dxCore, cxContainer, cxEdit, cxNavigator,
  cxDBNavigator, cxImage, cxDBEdit, cxHyperLinkEdit, cxTextEdit, cxMaskEdit,
  dxLayoutLookAndFeels;

type
  TfmFlexibleLayoutDemoMain = class(TForm)
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutLabeledItem1: TdxLayoutLabeledItem;
    dxLayoutImageItem1: TdxLayoutImageItem;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutItem5: TdxLayoutItem;
    cxDBImage1: TcxDBImage;
    dxLayoutItem6: TdxLayoutItem;
    cxDBNavigator1: TcxDBNavigator;
    dxLayoutItem7: TdxLayoutItem;
    cxDBTextEdit12: TcxDBTextEdit;
    dxLayoutItem8: TdxLayoutItem;
    cxDBTextEdit11: TcxDBTextEdit;
    dxLayoutItem9: TdxLayoutItem;
    cxDBHyperLinkEdit2: TcxDBHyperLinkEdit;
    dxLayoutItem1: TdxLayoutItem;
    cxDBTextEdit3: TcxDBTextEdit;
    dxLayoutItem2: TdxLayoutItem;
    cxDBTextEdit1: TcxDBTextEdit;
    dxLayoutItem3: TdxLayoutItem;
    cxDBTextEdit2: TcxDBTextEdit;
    dxLayoutItem4: TdxLayoutItem;
    cxDBTextEdit4: TcxDBTextEdit;
    dxLayoutItem10: TdxLayoutItem;
    cxDBTextEdit8: TcxDBTextEdit;
    dxLayoutItem11: TdxLayoutItem;
    cxDBTextEdit9: TcxDBTextEdit;
    dxLayoutItem12: TdxLayoutItem;
    cxDBMaskEdit1: TcxDBMaskEdit;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutStandardLookAndFeel1: TdxLayoutStandardLookAndFeel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure mdOrdersAfterScroll(DataSet: TDataSet);
  public
    { Public declarations }
  end;

var
  fmFlexibleLayoutDemoMain: TfmFlexibleLayoutDemoMain;

implementation

{$R *.dfm}

{ TfmFlexibleLayoutDemoMain }

procedure TfmFlexibleLayoutDemoMain.FormCreate(Sender: TObject);
begin
  dmDemo.mdOrders.AfterScroll := mdOrdersAfterScroll;
  mdOrdersAfterScroll(nil);
end;

procedure TfmFlexibleLayoutDemoMain.mdOrdersAfterScroll(DataSet: TDataSet);
begin
  dmDemo.mdCustomers.Locate('ID', dmDemo.mdOrdersCustomers_ID.Value, []);

  dxLayoutItem5.Caption := dxAnsiStringToString(
    dmDemo.mdOrdersFirstName.Value + ' ' + dmDemo.mdOrdersLastName.Value +
    #13#10 + dmDemo.mdCustomersHomePhone.Value +
    #13#10 + dmDemo.mdCustomersEmail.Value);

  dxLayoutLabeledItem1.Caption := dxAnsiStringToString(dmDemo.mdCustomersDescription.Value);
end;

end.
