unit SimpleTreeDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  cxStyles, cxTL, cxMaskEdit, cxCurrencyEdit, cxMemo, cxCheckBox, Menus,
  cxLookAndFeels, ActnList, ImgList, cxInplaceContainer, cxDBTL,
  cxControls, cxTLData, ComCtrls, StdCtrls, DemoBasicMain, cxGraphics,
  cxCustomData, Dialogs, cxLookAndFeelPainters;

type
  TSimpleTreeDemoMainForm = class(TDemoBasicMainForm)
    miFullCollapse: TMenuItem;
    miFullExpand: TMenuItem;
    miSeparator1: TMenuItem;
    cxDBTreeList: TcxDBTreeList;
    mnuNodeOptions: TPopupMenu;
    miNodeDelete: TMenuItem;
    N1: TMenuItem;
    miOptionsView: TMenuItem;
    miHeaders: TMenuItem;
    miIndicator: TMenuItem;
    miButtons: TMenuItem;
    miShowRoot: TMenuItem;
    miColumnCustomization: TMenuItem;
    miNodeAdd: TMenuItem;
    miNodeAddChild: TMenuItem;
    N2: TMenuItem;
    miExpand: TMenuItem;
    miCollapse: TMenuItem;
    miPreview: TMenuItem;
    cxDBTreeListID: TcxDBTreeListColumn;
    cxDBTreeListPARENTID: TcxDBTreeListColumn;
    cxDBTreeListNAME: TcxDBTreeListColumn;
    cxDBTreeListBUDGET: TcxDBTreeListColumn;
    cxDBTreeListPHONE: TcxDBTreeListColumn;
    cxDBTreeListFAX: TcxDBTreeListColumn;
    cxDBTreeListEMAIL: TcxDBTreeListColumn;
    cxDBTreeListVACANCY: TcxDBTreeListColumn;
    procedure FormShow(Sender: TObject);
    procedure mnuNodeOptionsPopup(Sender: TObject);
    procedure miFullCollapseClick(Sender: TObject);
    procedure miFullExpandClick(Sender: TObject);
    procedure miHeadersClick(Sender: TObject);
    procedure miIndicatorClick(Sender: TObject);
    procedure miButtonsClick(Sender: TObject);
    procedure miShowRootClick(Sender: TObject);
    procedure miColumnCustomizationClick(Sender: TObject);
    procedure miNodeDeleteClick(Sender: TObject);
    procedure miNodeAddClick(Sender: TObject);
    procedure miNodeAddChildClick(Sender: TObject);
    procedure miExpandClick(Sender: TObject);
    procedure miCollapseClick(Sender: TObject);
    procedure miPreviewClick(Sender: TObject);
    procedure cxDBTreeListInitInsertingRecord(Sender: TcxCustomDBTreeList;
      AFocusedNode: TcxDBTreeListNode; var AHandled: Boolean);
  private
    FHitNode: TcxDBTreeListNode;
    procedure InsertNode(AParentID: Integer);
  end;

var
  SimpleTreeDemoMainForm: TSimpleTreeDemoMainForm;

implementation

uses
  SimpleTreeDemoData, ShellAPI, DB, SkinDemoUtils;

{$R *.dfm}

procedure TSimpleTreeDemoMainForm.FormShow(Sender: TObject);
begin
  cxDBTreeList.FullExpand;
{ remove/add the closing brace on this line to disable/enable the following code

  ShowMessage('WARNING: tutorial not completed. First, please apply the steps '+
              'shown in the doc file');

//}
end;

procedure TSimpleTreeDemoMainForm.mnuNodeOptionsPopup(Sender: TObject);
begin
  if not cxDBTreeList.HitTest.HitAtIndicator then Abort;
  FHitNode := TcxDBTreeListNode(cxDBTreeList.HitTest.HitNode);
end;

procedure TSimpleTreeDemoMainForm.miFullCollapseClick(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  cxDBTreeList.FullCollapse;

//}
end;

procedure TSimpleTreeDemoMainForm.miFullExpandClick(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  cxDBTreeList.FullExpand;

//}
end;

procedure TSimpleTreeDemoMainForm.miHeadersClick(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

{$IFNDEF EXPRESSBARS}
  SetMenuItemChecked(Sender, not GetMenuItemChecked(Sender));
{$ENDIF}
  cxDBTreeList.OptionsView.Headers := GetMenuItemChecked(Sender);

//}
end;

procedure TSimpleTreeDemoMainForm.miIndicatorClick(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

{$IFNDEF EXPRESSBARS}
  SetMenuItemChecked(Sender, not GetMenuItemChecked(Sender));
{$ENDIF}
  cxDBTreeList.OptionsView.Indicator := GetMenuItemChecked(Sender);

//}
end;

procedure TSimpleTreeDemoMainForm.miButtonsClick(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

{$IFNDEF EXPRESSBARS}
  SetMenuItemChecked(Sender, not GetMenuItemChecked(Sender));
{$ENDIF}
  cxDBTreeList.OptionsView.Buttons := GetMenuItemChecked(Sender);

//}
end;

procedure TSimpleTreeDemoMainForm.miShowRootClick(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

{$IFNDEF EXPRESSBARS}
  SetMenuItemChecked(Sender, not GetMenuItemChecked(Sender));
{$ENDIF}
  cxDBTreeList.OptionsView.ShowRoot := GetMenuItemChecked(Sender);

//}
end;

procedure TSimpleTreeDemoMainForm.miColumnCustomizationClick(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  cxDBTreeList.Customizing.Visible := True;

//}
end;

procedure TSimpleTreeDemoMainForm.miNodeDeleteClick(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  FHitNode.Delete;

//}
end;

procedure TSimpleTreeDemoMainForm.miNodeAddClick(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  InsertNode(FHitNode.ParentKeyValue);

//}
end;

procedure TSimpleTreeDemoMainForm.miNodeAddChildClick(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  InsertNode(FHitNode.KeyValue);

//}
end;

procedure TSimpleTreeDemoMainForm.miExpandClick(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  FHitNode.Expand(True);

//}
end;

procedure TSimpleTreeDemoMainForm.miCollapseClick(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  FHitNode.Collapse(True);

//}
end;

procedure TSimpleTreeDemoMainForm.miPreviewClick(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

{$IFNDEF EXPRESSBARS}
  SetMenuItemChecked(Sender, not GetMenuItemChecked(Sender));
{$ENDIF}
  if GetMenuItemChecked(Sender) then
    cxDBTreeList.Preview.Column := cxDBTreeListNAME
  else
  begin
    cxDBTreeList.Preview.Column := nil;
    cxDBTreeListNAME.ApplyBestFit;
  end;

//}
end;

procedure TSimpleTreeDemoMainForm.InsertNode(AParentID: Integer);
var
  ADataSet: TDataSet;
  AField: TField;
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  ADataSet := cxDBTreeList.DataController.DataSet;
  ADataSet.DisableControls;
  try
    AField := ADataSet.FindField(cxDBTreeList.DataController.ParentField);
    if Assigned(AField) then
    begin
      ADataSet.Insert;
      AField.Value := AParentID;
    end;
  finally
    ADataSet.EnableControls;
  end;

//}
end;

procedure TSimpleTreeDemoMainForm.cxDBTreeListInitInsertingRecord(
  Sender: TcxCustomDBTreeList; AFocusedNode: TcxDBTreeListNode;
  var AHandled: Boolean);
begin
{ you can initialize key values via this event }
end;

end.
