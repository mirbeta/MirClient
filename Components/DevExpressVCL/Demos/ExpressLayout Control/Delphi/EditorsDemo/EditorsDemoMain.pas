unit EditorsDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dxLayoutControl, cxControls, StdCtrls, cxLookAndFeels,
  dxLayoutLookAndFeels, ExtCtrls, Menus, DB, DemoDM,
  dxLayoutControlAdapters, DBCtrls, Mask, Grids, DBGrids, BasicDemoMain,
  ActnList, cxGraphics, cxLookAndFeelPainters, ImgList, dxLayoutContainer;

type
  TfrmEditorsDemoMain = class(TfrmBasicDemoMain)
    lcMainGroup1: TdxLayoutGroup;
    lcMainGroup2: TdxLayoutGroup;
    lcMainGroup3: TdxLayoutGroup;
    lcMainGroup4: TdxLayoutGroup;
    lcMainGroup5: TdxLayoutGroup;
    lcMainGroup6: TdxLayoutGroup;
    DBEdit1: TDBEdit;
    lcMainItem1: TdxLayoutItem;
    DBEdit2: TDBEdit;
    lcMainItem2: TdxLayoutItem;
    DBEdit4: TDBEdit;
    lcMainItem5: TdxLayoutItem;
    DBEdit5: TDBEdit;
    lcMainItem6: TdxLayoutItem;
    DBEdit6: TDBEdit;
    lcMainItem7: TdxLayoutItem;
    DBEdit7: TDBEdit;
    lcMainItem8: TdxLayoutItem;
    lcMainGroup10: TdxLayoutGroup;
    DBEdit8: TDBEdit;
    lcMainItem9: TdxLayoutItem;
    DBEdit9: TDBEdit;
    lcMainItem10: TdxLayoutItem;
    DBCheckBox1: TDBCheckBox;
    lcMainItem11: TdxLayoutItem;
    DBEdit10: TDBEdit;
    lcMainItem12: TdxLayoutItem;
    DBEdit11: TDBEdit;
    lcMainItem13: TdxLayoutItem;
    DBEdit12: TDBEdit;
    lcMainItem14: TdxLayoutItem;
    DBEdit13: TDBEdit;
    lcMainItem15: TdxLayoutItem;
    DBEdit14: TDBEdit;
    lcMainItem16: TdxLayoutItem;
    DBEdit15: TDBEdit;
    lcMainItem17: TdxLayoutItem;
    DBEdit16: TDBEdit;
    lcMainItem18: TdxLayoutItem;
    DBEdit17: TDBEdit;
    lcMainItem19: TdxLayoutItem;
    DBEdit18: TDBEdit;
    lcMainItem20: TdxLayoutItem;
    lcMainGroup13: TdxLayoutGroup;
    DBEdit19: TDBEdit;
    lcMainItem21: TdxLayoutItem;
    DBEdit20: TDBEdit;
    lcMainItem22: TdxLayoutItem;
    DBEdit21: TDBEdit;
    lcMainItem23: TdxLayoutItem;
    DBEdit22: TDBEdit;
    lcMainItem24: TdxLayoutItem;
    DBEdit23: TDBEdit;
    lcMainItem25: TdxLayoutItem;
    DBEdit24: TDBEdit;
    lcMainItem26: TdxLayoutItem;
    DBEdit25: TDBEdit;
    lcMainItem27: TdxLayoutItem;
    DBEdit26: TDBEdit;
    lcMainItem28: TdxLayoutItem;
    DBEdit27: TDBEdit;
    lcMainItem29: TdxLayoutItem;
    DBCheckBox2: TDBCheckBox;
    lcMainItem30: TdxLayoutItem;
    DBEdit28: TDBEdit;
    lcMainItem31: TdxLayoutItem;
    lcMainGroup14: TdxLayoutGroup;
    lcMainGroup15: TdxLayoutGroup;
    DBMemo1: TDBMemo;
    lcMainItem32: TdxLayoutItem;
    DBNavigator1: TDBNavigator;
    lcMainItem33: TdxLayoutItem;
    lcMainItem34: TdxLayoutItem;
    DBComboBox2: TDBComboBox;
    lcMainItem4: TdxLayoutItem;
    DBEdit29: TDBEdit;
    lcMainGroup16: TdxLayoutGroup;
    lcMainGroup17: TdxLayoutGroup;
    cxImageList1: TcxImageList;
    lcMainSeparatorItem1: TdxLayoutSeparatorItem;
    lcMainSplitterItem1: TdxLayoutSplitterItem;
    lcMainSplitterItem2: TdxLayoutSplitterItem;
    lcMainSeparatorItem2: TdxLayoutSeparatorItem;
    lcMainGroup7: TdxLayoutGroup;
    lcMainGroup8: TdxLayoutGroup;
    lcMainGroup9: TdxLayoutGroup;
    dxLayoutSplitterItem1: TdxLayoutSplitterItem;
    procedure lcMainGroup2Button0Click(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  frmEditorsDemoMain: TfrmEditorsDemoMain;

implementation

{$R *.dfm}

procedure TfrmEditorsDemoMain.lcMainGroup2Button0Click(Sender: TObject);
begin
  inherited;
  lcMainGroup2.Parent := nil;
end;

end.
