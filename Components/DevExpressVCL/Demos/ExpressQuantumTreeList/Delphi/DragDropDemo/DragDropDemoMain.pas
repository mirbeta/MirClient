unit DragDropDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  cxStyles, cxTL, cxMaskEdit, cxCurrencyEdit, cxMemo, cxCheckBox, Menus,
  cxLookAndFeels, ActnList, ImgList, cxInplaceContainer, cxDBTL,
  cxControls, cxTLData, ComCtrls, StdCtrls, DemoBasicMain, ExtCtrls,
  cxGraphics, cxCustomData, Dialogs, cxLookAndFeelPainters;

type
  TcxDragSenderType = (dotDepart, dotPers, dotNone);

  TDragDropDemoMainForm = class(TDemoBasicMainForm)
    miSeparator1: TMenuItem;
    miColumnCustomization: TMenuItem;
    Splitter1: TSplitter;
    pnlDepartments: TPanel;
    tlDepartments: TcxDBTreeList;
    tlDepartmentsID: TcxDBTreeListColumn;
    tlDepartmentsPARENTID: TcxDBTreeListColumn;
    tlDepartmentsNAME: TcxDBTreeListColumn;
    tlDepartmentsBUDGET: TcxDBTreeListColumn;
    tlDepartmentsPHONE: TcxDBTreeListColumn;
    tlDepartmentsFAX: TcxDBTreeListColumn;
    tlDepartmentsEMAIL: TcxDBTreeListColumn;
    tlDepartmentsVACANCY: TcxDBTreeListColumn;
    pnlDeptCaption: TPanel;
    pnlEmployees: TPanel;
    pnlEmplCaption: TPanel;
    tlEmployees: TcxDBTreeList;
    tlEmployeesName: TcxDBTreeListColumn;
    tlEmployeesCountry: TcxDBTreeListColumn;
    tlEmployeesPostalCode: TcxDBTreeListColumn;
    tlEmployeesCity: TcxDBTreeListColumn;
    tlEmployeesAddress: TcxDBTreeListColumn;
    tlEmployeesPhone: TcxDBTreeListColumn;
    tlEmployeesFax: TcxDBTreeListColumn;
    tlEmployeesEMAIL: TcxDBTreeListColumn;
    tlEmployeesHOMEPAGE: TcxDBTreeListColumn;
    tlEmployeesDepartmentID: TcxDBTreeListColumn;
    N1: TMenuItem;
    miDragExpande: TMenuItem;
    miDragCollapse: TMenuItem;
    N2: TMenuItem;
    ShowDictionaries1: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure miColumnCustomizationClick(Sender: TObject);
    procedure miDragCollapseClick(Sender: TObject);
    procedure miDragExpandeClick(Sender: TObject);
    procedure ShowDictionaries1Click(Sender: TObject);
    procedure tlDepartmentsInitInsertingRecord(Sender: TcxCustomDBTreeList;
      AFocusedNode: TcxDBTreeListNode; var AHandled: Boolean);
    procedure tlDepartmentsDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure tlDepartmentsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tlEmployeesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tlEmployeesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tlEmployeesMoveTo(Sender: TcxCustomTreeList;
      AttachNode: TcxTreeListNode; AttachMode: TcxTreeListNodeAttachMode;
      Nodes: TList; var IsCopy, Done: Boolean);
  private
    function IsHitAtNode(ATreeList: TcxDBTreeList; X, Y: Integer): Boolean;
    procedure SetEmplDictSelectionDeptID(AValue: Variant);
  end;

var
  DragDropDemoMainForm: TDragDropDemoMainForm;

implementation

uses
  DragDropDemoData, ShellAPI, DB, DragDropDemoDictionary, SkinDemoUtils;

{$R *.dfm}

procedure TDragDropDemoMainForm.FormShow(Sender: TObject);
begin
  tlDepartments.FullExpand;
  DragDropDemoDictionaryForm.Show;

{ remove/add the closing brace on this line to disable/enable the following code

  ShowMessage('WARNING: tutorial not completed. First, please apply the steps '+
              'shown in the doc file');

//}
end;

procedure TDragDropDemoMainForm.miColumnCustomizationClick(Sender: TObject);
begin
  tlDepartments.Customizing.Visible := True;
end;

procedure TDragDropDemoMainForm.miDragCollapseClick(Sender: TObject);
begin
{$IFNDEF EXPRESSBARS}
  SetMenuItemChecked(Sender, not GetMenuItemChecked(Sender));
{$ENDIF}
  tlDepartments.OptionsBehavior.DragCollapse := GetMenuItemChecked(Sender);
end;

procedure TDragDropDemoMainForm.miDragExpandeClick(Sender: TObject);
begin
{$IFNDEF EXPRESSBARS}
  SetMenuItemChecked(Sender, not GetMenuItemChecked(Sender));
{$ENDIF}
  tlDepartments.OptionsBehavior.DragExpand := GetMenuItemChecked(Sender);
end;

procedure TDragDropDemoMainForm.ShowDictionaries1Click(Sender: TObject);
begin
  DragDropDemoDictionaryForm.Show;
end;

procedure TDragDropDemoMainForm.tlDepartmentsInitInsertingRecord(
  Sender: TcxCustomDBTreeList; AFocusedNode: TcxDBTreeListNode;
  var AHandled: Boolean);
begin
  if AFocusedNode <> nil then
    DragDropDemoDataDM.SetParentValue(AFocusedNode.ParentKeyValue);
end;

procedure TDragDropDemoMainForm.tlDepartmentsDragDrop(Sender,
  Source: TObject; X, Y: Integer);
  function IsDropAsChild: Boolean;
  begin
    with TcxDBTreeList(Sender) do
      Result := not (HitTest.HitAtIndent or HitTest.HitAtIndicator)
  end;
var
  AHitNode: TcxDBTreeListNode;
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  if (Sender = Source) or not IsHitAtNode(TcxDBTreeList(Sender), X, Y) then
    Exit;
  AHitNode := TcxDBTreeListNode(TcxDBTreeList(Sender).HitTest.HitNode);
  with DragDropDemoDictionaryForm do
    if Source = tlDeptDict then
    begin
      if IsDropAsChild then
        SetDeptSelectionParentValue(AHitNode.KeyValue)
      else
        SetDeptSelectionParentValue(AHitNode.ParentKeyValue);
      TcxDBTreeList(Sender).DataController.DataSet.Refresh;
    end
    else
    begin
      if Source = tlEmplDict then
        SetEmplDictSelectionDeptID(AHitNode.KeyValue)
      else
        SetSelectedNodesValue(tlEmployees, tlEmployeesDepartmentID.ItemIndex,
          AHitNode.KeyValue);
    end;

//}
end;

procedure TDragDropDemoMainForm.tlDepartmentsDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  Accept := IsHitAtNode(TcxDBTreeList(Sender), X, Y);

//}
end;

procedure TDragDropDemoMainForm.tlEmployeesDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  AValue: Variant;
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  if Source = DragDropDemoDictionaryForm.tlEmplDict then
  begin
    if tlDepartments.FocusedNode <> nil then
      AValue := TcxDBTreeListNode(tlDepartments.FocusedNode).KeyValue
    else
      AValue := -1;
    SetEmplDictSelectionDeptID(AValue)
  end;

//}
end;

procedure TDragDropDemoMainForm.tlEmployeesDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  Accept := (Source = DragDropDemoDictionaryForm.tlEmplDict);

//}
end;

procedure TDragDropDemoMainForm.tlEmployeesMoveTo(Sender: TcxCustomTreeList;
  AttachNode: TcxTreeListNode; AttachMode: TcxTreeListNodeAttachMode;
  Nodes: TList; var IsCopy, Done: Boolean);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  Done := True;

//}
end;

function TDragDropDemoMainForm.IsHitAtNode(ATreeList: TcxDBTreeList; X,
  Y: Integer): Boolean;
begin
  with ATreeList do
  begin
    HitTest.ReCalculate(Point(X,Y));
    Result := HitTest.HitAtNode;
  end;
end;

procedure TDragDropDemoMainForm.SetEmplDictSelectionDeptID(AValue: Variant);
begin
  DragDropDemoDictionaryForm.SetEmplSelectionDeptID(AValue);
  tlEmployees.DataController.DataSet.Close;
  tlEmployees.DataController.DataSet.Open;
end;

end.
