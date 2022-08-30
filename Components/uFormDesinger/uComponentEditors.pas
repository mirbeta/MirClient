unit uComponentEditors;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  ExtCtrls, StdCtrls, Menus, ComCtrls, Variants, typinfo,
  uDesignIntf, uDesigner, uDesignEditors, ActnPopup
  , PlatformDefaultStyleActnCtrls;

type
  TPopupEditors = class
  private
    EditorFlagStart: Integer;
    FDefaultCount:  Integer;  //Ĭ���ж��ٸ�ѡ��
    FDesigner: TCustomDesigner;
    FPopupmenu: TPopupActionBar;
    FCompEditor: IComponentEditor;
    procedure CreateDefaultPopupMenu;
    procedure doDefaultMenuItemClick(Sender: TObject);
    procedure CreateComponentEditor(AComponent: TComponent);
    procedure SimulateComponentEditor(Sender: TObject);
  public
    constructor Create(ADesigner: TCustomDesigner);
    destructor destroy; override;
    procedure Edit(const Selection: IDesignerSelections);
    property Designer: TCustomDesigner read FDesigner write FDesigner;
  end;

implementation
  uses uControlPostionSize, uTabOrderEditor, uDesignMenusFactor, uImgEdit;

{ TPopupEditors }
constructor TPopupEditors.Create(ADesigner: TCustomDesigner);
begin
  EditorFlagStart :=  255;
  FDesigner   :=  ADesigner;
  FPopupmenu  :=  TPopupActionBar.Create(nil);
  FPopupMenu.AutoHotkeys  :=  maManual;
end;

destructor TPopupEditors.destroy;
begin
  FreeAndNil(FPopupmenu);
  FDesigner :=  nil;
  inherited;
end;

procedure TPopupEditors.CreateComponentEditor(AComponent: TComponent);
var
  iceItem:  TMenuItem;
  I: Integer;
  mfItem: TMenuItemFactor;
begin
  FCompEditor :=  GetComponentEditor(AComponent, FDesigner);
  if FCompEditor<>nil then
    for I := 0 to FCompEditor.GetVerbCount-1 do
    begin
      iceItem         :=  TMenuItem.Create(FPopupmenu);
      iceItem.Tag     :=  I + EditorFlagStart;
      iceItem.Caption :=  FCompEditor.GetVerb(I);
      iceItem.OnClick :=  SimulateComponentEditor;
      mfItem          :=  TMenuItemFactor.Create(iceItem);
      FCompEditor.PrepareItem(I, mfItem);
      FPopupmenu.Items.Add(iceItem);
      mfItem.Free;
    end;
end;

procedure TPopupEditors.CreateDefaultPopupMenu;
  function AddNoActionMenuItem: TMenuItem;
  begin
    result  :=  TMenuItem.Create(FPopupmenu);
    result.Caption  :=  '-';
  end;
var
//Ĭ�����������˵�
  PNone,  //'-'
  pControlPostion,     //λ�ô�С����
  pCPSize,              //���ƴ�С
  pTableOrder,        //tab˳�����
  PControl,           //��ǰ�ú�λ�ÿ���
  pcFront,
  pcBack,
  pEdit,              //�༭���
  pCopy,              //-����
  pCut,               //-����
  pPaste,             //-ճ��
  pDelete: TMenuItem; //-ɾ��
begin
  if FDesigner.SelectionCount = 0 then Exit;
  if FPopupmenu.Items.Count > 0 then
    FPopupmenu.Items.Add(AddNoActionMenuItem);

  pEdit :=  TMenuItem.Create(FPopupmenu);
  pEdit.Caption :=  '�༭';
  FPopupmenu.Items.Add(pEdit);

  pCopy :=  TMenuItem.Create(FPopupmenu);
  pCopy.Caption :=  '����';
  pCopy.Tag     :=  1;
  pCopy.OnClick :=  doDefaultMenuItemClick;
  pCopy.Enabled :=  FDesigner.CanCopyElements;
  pEdit.Add(pCopy);

  pCut :=  TMenuItem.Create(FPopupmenu);
  pCut.Caption :=  '����';
  pCut.Tag     :=  2;
  pCut.OnClick :=  doDefaultMenuItemClick;
  pCut.Enabled :=  FDesigner.CanCutElements;
  pEdit.Add(pCut);

  pPaste :=  TMenuItem.Create(FPopupmenu);
  pPaste.Caption :=  'ճ��';
  pPaste.Tag     :=  3;
  pPaste.OnClick :=  doDefaultMenuItemClick;
  pPaste.Enabled :=  FDesigner.CanPasteElements;
  pEdit.Add(pPaste);

  pDelete :=  TMenuItem.Create(FPopupmenu);
  pDelete.Caption :=  'ɾ��';
  pDelete.Tag     :=  4;
  pDelete.OnClick :=  doDefaultMenuItemClick;
  pDelete.Enabled :=  FDesigner.CanDeleteElements;
  pEdit.Add(pDelete);

  pTableOrder := TMenuItem.Create(FPopupmenu);
  pTableOrder.Caption :=  'Tab˳��';
  pTableOrder.Tag     :=  5;
  pTableOrder.OnClick :=  doDefaultMenuItemClick;
  FPopupmenu.Items.Add(pTableOrder);
  if FDesigner.Selections[0] <> FDesigner.DesignForm then
  begin
    if FDesigner.SelectionCount = 1 then
    begin
      PControl            := TMenuItem.Create(FPopupmenu);
      PControl.Caption    :=  '�ؼ�';
      FPopupmenu.Items.Add(PControl);
      pcFront         := TMenuItem.Create(PControl);
      pcFront.Caption :=  'ͣ�ŵ�ǰ';
      pcFront.Tag     :=  6;
      pcFront.OnClick :=  doDefaultMenuItemClick;
      pcBack          := TMenuItem.Create(PControl);
      pcBack.Caption  :=  'ͣ�ŵ���';
      pcBack.OnClick  :=  doDefaultMenuItemClick;
      pcBack.Tag      :=  7;
      PControl.Add(pcFront);
      PControl.Add(pcBack);
    end;
    pControlPostion         :=  TMenuItem.Create(FPopupmenu);
    pControlPostion.Caption :=  'λ�úʹ�С';
    FPopupmenu.Items.Add(pControlPostion);
    pCPSize :=  TMenuItem.Create(pControlPostion);
    pCPSize.Caption :=  '�ؼ���С����';
    pCPSize.Tag :=  8;
    pCPSize.OnClick :=  doDefaultMenuItemClick;
    pControlPostion.Add(pCPSize);
  end;
end;

procedure TPopupEditors.doDefaultMenuItemClick(sender: TObject);
var
  frmPostionSize: TfrmPostionSize;
  frmTabOrder: TfrmTabOrder;
begin
  case TMenuitem(Sender).Tag of
    1:  FDesigner.Copy;
    2:  FDesigner.Cut;
    3:  FDesigner.Paste;
    4:  FDesigner.Delete;
    5: begin  //����tab˳��
      try
        frmTabOrder := TfrmTabOrder.create(nil, FDesigner);
        frmTabOrder.ShowModal;
      finally
        frmTabOrder.Free;
      end;
    end;
    6: if FDesigner.Selections[0] is TControl then
      TControl(FDesigner.Selections[0]).BringToFront;
    7: if FDesigner.Selections[0] is TControl then
      TControl(FDesigner.Selections[0]).SendToBack;
    8:begin
      try
        frmPostionSize  := TfrmPostionSize.Create(nil, FDesigner);
        frmPostionSize.ShowModal;
      finally
        frmPostionSize.Free;
      end;
    end;
  end;
end;

procedure TPopupEditors.Edit(const Selection: IDesignerSelections);
var
  p: TPoint;
begin
  if Selection.Count=0 then Exit;
  FPopupMenu.Items.Clear;

  if (Selection.Count=1) and (Selection.Items[0] is TComponent) then
    CreateComponentEditor(Selection.Items[0] as TComponent);

  CreateDefaultPopupMenu;
  GetCursorPos(p);
  FPopupmenu.Popup(p.X, p.Y);
end;

procedure TPopupEditors.SimulateComponentEditor(Sender: TObject);
begin
  if FCompEditor<>nil then
    FCompEditor.ExecuteVerb(TMenuItem(Sender).Tag-EditorFlagStart);
end;

end.
