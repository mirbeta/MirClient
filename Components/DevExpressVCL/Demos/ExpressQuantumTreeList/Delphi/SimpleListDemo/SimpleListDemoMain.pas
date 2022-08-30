unit SimpleListDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, cxStyles, cxTL, cxMaskEdit, cxBlobEdit,
  cxCurrencyEdit, cxSpinEdit, cxCheckBox, cxHyperLinkEdit, cxEditRepositoryItems, cxEdit, cxLookAndFeels, ActnList,
  ImgList, Menus, cxInplaceContainer, cxDBTL, cxControls, cxTLData, ComCtrls, StdCtrls, DemoBasicMain, cxGraphics,
  cxCustomData, Dialogs, cxLookAndFeelPainters, CarsData, cxClasses, cxDBLookupComboBox;

type
  TSimpleListDemoMainForm = class(TDemoBasicMainForm)
    miView: TMenuItem;
    miBehavior: TMenuItem;
    miSeparator1: TMenuItem;
    cxDBTreeList: TcxDBTreeList;
    cxEditRepository: TcxEditRepository;
    cxEditRepositorySpinItem: TcxEditRepositorySpinItem;
    eriPicture: TcxEditRepositoryBlobItem;
    eriHP: TcxEditRepositorySpinItem;
    eriDescription: TcxEditRepositoryBlobItem;
    eriURL: TcxEditRepositoryHyperLinkItem;
    cxDBTreeListID: TcxDBTreeListColumn;
    cxDBTreeListTrademark: TcxDBTreeListColumn;
    cxDBTreeListModel: TcxDBTreeListColumn;
    cxDBTreeListPicture: TcxDBTreeListColumn;
    cxDBTreeListPrice: TcxDBTreeListColumn;
    cxDBTreeListHP: TcxDBTreeListColumn;
    cxDBTreeListTorque: TcxDBTreeListColumn;
    cxDBTreeListCyl: TcxDBTreeListColumn;
    cxDBTreeListTransmissSpeedCount: TcxDBTreeListColumn;
    cxDBTreeListTransmissAutomatic: TcxDBTreeListColumn;
    cxDBTreeListMPG_City: TcxDBTreeListColumn;
    cxDBTreeListMPG_Highway: TcxDBTreeListColumn;
    cxDBTreeListCategory: TcxDBTreeListColumn;
    cxDBTreeListHyperlink: TcxDBTreeListColumn;
    cxDBTreeListDescription: TcxDBTreeListColumn;
    miBands: TMenuItem;
    miHeaders: TMenuItem;
    miGridLines: TMenuItem;
    miIncSearch: TMenuItem;
    miFocusCellOnCycle: TMenuItem;
    miImmediateEditor: TMenuItem;
    miMultiSelect: TMenuItem;
    procedure miBandsClick(Sender: TObject);
    procedure miHeadersClick(Sender: TObject);
    procedure miGridLinesClick(Sender: TObject);
    procedure miIncSearchClick(Sender: TObject);
    procedure miFocusCellOnCycleClick(Sender: TObject);
    procedure miImmediateEditorClick(Sender: TObject);
    procedure miMultiSelectClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  end;

var
  SimpleListDemoMainForm: TSimpleListDemoMainForm;

implementation

uses
  SimpleListDemoData, ShellAPI, SkinDemoUtils;

{$R *.dfm}

procedure TSimpleListDemoMainForm.FormShow(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code

  ShowMessage('WARNING: tutorial not completed. First, please apply the steps '+
              'shown in the doc file');

//}
end;

procedure TSimpleListDemoMainForm.miBandsClick(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  cxDBTreeList.OptionsView.Bands := GetMenuItemChecked(Sender);

//}
end;

procedure TSimpleListDemoMainForm.miHeadersClick(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  cxDBTreeList.OptionsView.Headers := GetMenuItemChecked(Sender);

//}
end;

procedure TSimpleListDemoMainForm.miGridLinesClick(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  if GetMenuItemChecked(Sender) then
    cxDBTreeList.OptionsView.GridLines := tlglBoth
  else
    cxDBTreeList.OptionsView.GridLines := tlglNone;

//}
end;

procedure TSimpleListDemoMainForm.miIncSearchClick(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  cxDBTreeList.OptionsBehavior.IncSearch := GetMenuItemChecked(Sender);

//}
end;

procedure TSimpleListDemoMainForm.miFocusCellOnCycleClick(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  cxDBTreeList.OptionsBehavior.FocusCellOnCycle := GetMenuItemChecked(Sender);

//}
end;

procedure TSimpleListDemoMainForm.miImmediateEditorClick(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  cxDBTreeList.OptionsBehavior.ImmediateEditor := GetMenuItemChecked(Sender);

//}
end;

procedure TSimpleListDemoMainForm.miMultiSelectClick(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  cxDBTreeList.OptionsSelection.MultiSelect := GetMenuItemChecked(Sender);

//}
end;

end.
