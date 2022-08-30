unit AdvPreviewMenuEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, Buttons, AdvPreviewMenu, ComCtrls, ExtCtrls, Math,
  ExtDlgs, ImgList, Menus;

type
  TPreviewMenuEditor = class(TForm)
    gb_OfficeHint: TGroupBox;
    mem_Notes: TMemo;
    edt_HintTitle: TEdit;
    lbl_HintTitle: TLabel;
    btn_HintPicture: TButton;
    chk_HintShowHelp: TCheckBox;
    lbl_HintNotes: TLabel;
    chk_CanSelect: TCheckBox;
    chk_Visible: TCheckBox;
    chk_Enabled: TCheckBox;
    chk_Separator: TCheckBox;
    edt_Caption: TEdit;
    lbl_Caption: TLabel;
    cmb_ImageIndex: TComboBox;
    lbl_ImageIndex: TLabel;
    btn_Picture: TButton;
    btn_DisabledPic: TButton;
    edt_SubMenuCaption: TEdit;
    lbl_SubMenuCaption: TLabel;
    spn_SubMenuSpacing: TSpinEdit;
    Label1: TLabel;
    edt_ShortCutHint: TEdit;
    lbl_ShortCutHint: TLabel;
    edt_ShortCutSubHint: TEdit;
    lbl_ShortCutSubHint: TLabel;
    spn_Tag: TSpinEdit;
    lbl_Tag: TLabel;
    lbl_Title: TLabel;
    lbl_SubImageIndex: TLabel;
    lbl_SubShortCutHint: TLabel;
    lbl_SubTag: TLabel;
    gb_SubOfficeHint: TGroupBox;
    lbl_SubHintTile: TLabel;
    lbl_SubHintNotes: TLabel;
    mem_SubHintNotes: TMemo;
    edt_SubHintTitle: TEdit;
    btn_SubHintPicture: TButton;
    chk_SubShowHelp: TCheckBox;
    chk_SubVisible: TCheckBox;
    chk_SubEnabled: TCheckBox;
    chk_SubSeparator: TCheckBox;
    edt_Title: TEdit;
    cmb_SubImageIndex: TComboBox;
    btn_SubPicture: TButton;
    btn_SubDisabledPic: TButton;
    edt_SubShortCutHint: TEdit;
    spn_SubTag: TSpinEdit;
    mem_SubNotes: TMemo;
    lbl_SubNotes: TLabel;
    btn_SubItemAdd: TButton;
    btn_SubItemRemove: TButton;
    btn_Add: TButton;
    btn_Remove: TButton;
    pnl_Items: TPanel;
    pg_Items: TPageControl;
    ts_MenuItems: TTabSheet;
    ts_SubMenuItems: TTabSheet;
    btn_RemoveDefault: TButton;
    btn_AddDefault: TButton;
    OpenDialog: TOpenPictureDialog;
    cmb_Action: TComboBox;
    lbl_Action: TLabel;
    cmb_SubAction: TComboBox;
    lbl_SubAction: TLabel;
    btn_Ok: TButton;
    Button1: TButton;
    cmb_SubMenu: TComboBox;
    lbl_SubMenu: TLabel;
    spn_SubMenuHeight: TSpinEdit;
    lbl_SubMenuHeight: TLabel;
    Label2: TLabel;
    pnl_Left: TPanel;
    btn_Up: TButton;
    btn_Down: TButton;
    procedure FormCreate(Sender: TObject);
    procedure cmb_ImageIndexChange(Sender: TObject);
    procedure spn_SubMenuSpacingChange(Sender: TObject);
    procedure spn_TagChange(Sender: TObject);
    procedure mem_NotesChange(Sender: TObject);
    procedure chk_CanSelectClick(Sender: TObject);
    procedure chk_VisibleClick(Sender: TObject);
    procedure chk_EnabledClick(Sender: TObject);
    procedure chk_SeparatorClick(Sender: TObject);
    procedure chk_HintShowHelpClick(Sender: TObject);
    procedure edt_CaptionKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edt_SubMenuCaptionKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edt_ShortCutHintKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edt_ShortCutSubHintKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edt_HintTitleKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btn_AddClick(Sender: TObject);
    procedure btn_RemoveClick(Sender: TObject);
    procedure edt_TitleKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edt_SubShortCutHintKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cmb_SubImageIndexChange(Sender: TObject);
    procedure spn_SubTagChange(Sender: TObject);
    procedure chk_SubVisibleClick(Sender: TObject);
    procedure chk_SubEnabledClick(Sender: TObject);
    procedure chk_SubSeparatorClick(Sender: TObject);
    procedure mem_SubNotesChange(Sender: TObject);
    procedure edt_SubHintTitleKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure mem_SubHintNotesChange(Sender: TObject);
    procedure chk_SubShowHelpClick(Sender: TObject);
    procedure btn_SubItemAddClick(Sender: TObject);
    procedure btn_SubItemRemoveClick(Sender: TObject);
    procedure btn_AddDefaultClick(Sender: TObject);
    procedure btn_RemoveDefaultClick(Sender: TObject);
    procedure btn_PictureClick(Sender: TObject);
    procedure btn_DisabledPicClick(Sender: TObject);
    procedure btn_SubPictureClick(Sender: TObject);
    procedure btn_SubDisabledPicClick(Sender: TObject);
    procedure cmb_SubImageIndexDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure cmb_ImageIndexDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cmb_ActionChange(Sender: TObject);
    procedure cmb_SubActionChange(Sender: TObject);
    procedure cmb_SubMenuChange(Sender: TObject);
    procedure spn_SubMenuHeightChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btn_UpClick(Sender: TObject);
    procedure btn_DownClick(Sender: TObject);
  private
    { Private declarations }
    FPreviewMenuControl: TPreviewMenuControl;
    FInternalItemChange: Boolean;
    FInternalSubItemChange: Boolean;
    procedure OnMenuItemClick(Sender: TObject; ItemIndex: Integer);
    procedure OnSubMenuItemClick(Sender: TObject; ItemIndex, SubMenuItemIndex: Integer);
    procedure OnPreviewMenuControlResize(Sender: TObject);
    procedure UpdatePreview;
  protected
    procedure Loaded; override;
  public
    { Public declarations }
    function ShowModal: Integer; override;
    property PreviewMenuControl: TPreviewMenuControl read FPreviewMenuControl;
  end;

var
  PreviewMenuEditor: TPreviewMenuEditor;

implementation

{$R *.dfm}

type
  TProPreviewMenuPanel = class(TAdvPreviewMenuPanel);

//------------------------------------------------------------------------------
  
procedure TPreviewMenuEditor.FormCreate(Sender: TObject);
begin
  FPreviewMenuControl := TPreviewMenuControl.Create(Self);
  FPreviewMenuControl.Parent := Self;
  FPreviewMenuControl.Left := 0;
  FPreviewMenuControl.Top := 0;
  FPreviewMenuControl.Visible := True;
  FPreviewMenuControl.AdvPreviewMenu.OnMenuItemClick := OnMenuItemClick;
  FPreviewMenuControl.AdvPreviewMenu.OnSubMenuItemClick := OnSubMenuItemClick;
  FPreviewMenuControl.OnResize := OnPreviewMenuControlResize;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.FormResize(Sender: TObject);
begin
  pnl_Left.Top := Max(FPreviewMenuControl.Top + FPreviewMenuControl.Height, ClientHeight - pnl_Left.Height - 4);
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.OnPreviewMenuControlResize(Sender: TObject);
var
  h: Integer;
begin
  h := Max(FPreviewMenuControl.Height + pnl_Left.Height, pnl_Items.Height);
  Height := h + 40;
  Width  := FPreviewMenuControl.Width + pnl_Items.Width + 15;
  pnl_Items.Left := FPreviewMenuControl.Left + FPreviewMenuControl.Width + 4;
  pnl_Left.Left := FPreviewMenuControl.Left;
  pnl_Left.Width := pnl_Items.Left - 1;
  pnl_Left.Top := ClientHeight - pnl_Left.Height - 4;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.btn_UpClick(Sender: TObject);
begin
  FPreviewMenuControl.MoveMenuItemUp;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.btn_DownClick(Sender: TObject);
begin
  FPreviewMenuControl.MoveMenuItemDown;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.OnMenuItemClick(Sender: TObject;
  ItemIndex: Integer);
begin
  FInternalItemChange := True;
  pg_Items.ActivePage := ts_MenuItems;
  if (ItemIndex < 0) then
  begin
    edt_Caption.Text := '';
    edt_SubMenuCaption.Text := '';
    edt_ShortCutHint.Text := '';
    edt_ShortCutSubHint.Text := '';
    if cmb_ImageIndex.Enabled and (cmb_ImageIndex.Items.Count > 0) then
      cmb_ImageIndex.Text := InttoStr(-1);
    spn_SubMenuSpacing.Value := 0;
    spn_Tag.Value := 0;
    spn_SubMenuHeight.Value := 0;
    chk_CanSelect.Checked := False;
    chk_Visible.Checked := False;
    chk_Enabled.Checked := False;
    chk_Separator.Checked := False;

    edt_HintTitle.Text := '';
    mem_Notes.Lines.Clear;
    chk_HintShowHelp.Checked := False;
    btn_AddDefault.Enabled := False;
    btn_RemoveDefault.Enabled := False;

    cmb_Action.Text := '';
    ts_SubMenuItems.Enabled := False;
  end
  else
  begin
    with FPreviewMenuControl.AdvPreviewMenu.MenuItems[ItemIndex] do
    begin
      ts_SubMenuItems.Enabled := True;
      edt_Caption.Text := Caption;
      edt_SubMenuCaption.Text := SubMenuCaption;
      edt_ShortCutHint.Text := ShortCutHint;
      edt_ShortCutSubHint.Text := ShortCutSubItemsHint;
      cmb_ImageIndex.Text := InttoStr(ImageIndex);
      spn_SubMenuSpacing.Value := SubMenuItemSpacing;
      spn_Tag.Value := Tag;
      spn_SubMenuHeight.Value := SubMenuItemHeight;
      chk_CanSelect.Checked := CanSelect;
      chk_Visible.Checked := Visible;
      chk_Enabled.Checked := Enabled;
      chk_Separator.Checked := Separator;
      if cmb_ImageIndex.Enabled and (ImageIndex < cmb_ImageIndex.Items.Count) then
      begin
        cmb_ImageIndex.ItemIndex := ImageIndex;
      end;
      edt_HintTitle.Text := FPreviewMenuControl.AdvPreviewMenu.MenuItems[ItemIndex].OfficeHint.title;
      mem_Notes.Lines.Clear;
      mem_Notes.Lines.AddStrings(FPreviewMenuControl.AdvPreviewMenu.MenuItems[ItemIndex].OfficeHint.Notes);
      chk_HintShowHelp.Checked := FPreviewMenuControl.AdvPreviewMenu.MenuItems[ItemIndex].OfficeHint.ShowHelp;

      btn_AddDefault.Enabled := (SubItems.Count = 0);
      btn_RemoveDefault.Enabled := (SubItems.Count = 0);
      if Assigned(FPreviewMenuControl.AdvPreviewMenu.MenuItems[ItemIndex].Action) then
        cmb_Action.Text := FPreviewMenuControl.AdvPreviewMenu.MenuItems[ItemIndex].Action.Name
      else
        cmb_Action.Text := '';
    end;
  end;
  FInternalItemChange := False;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.cmb_ImageIndexChange(Sender: TObject);
var
  i: Integer;
begin
  if FInternalItemChange then
    Exit;

  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  if (i >= 0) then
  begin
    FPreviewMenuControl.AdvPreviewMenu.MenuItems[i].ImageIndex := StrToInt(cmb_ImageIndex.Items[cmb_ImageIndex.ItemIndex]);
    UpdatePreview;
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).DrawItem(i);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.spn_SubMenuSpacingChange(Sender: TObject);
var
  i: Integer;
begin
  if FInternalItemChange then
    Exit;
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  if (i >= 0) then
  begin
    FPreviewMenuControl.AdvPreviewMenu.MenuItems[i].SubMenuItemSpacing := spn_SubMenuSpacing.Value;;
    UpdatePreview;
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).DrawItem(i);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.spn_TagChange(Sender: TObject);
var
  i: Integer;
begin
  if FInternalItemChange then
    Exit;
    
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  if (i >= 0) then
    FPreviewMenuControl.AdvPreviewMenu.MenuItems[i].Tag := spn_Tag.Value;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.mem_NotesChange(Sender: TObject);
var
  i: Integer;
begin
  if FInternalItemChange then
    Exit;
    
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  if (i >= 0) then
  begin
    FPreviewMenuControl.AdvPreviewMenu.MenuItems[i].OfficeHint.Notes.Clear;
    FPreviewMenuControl.AdvPreviewMenu.MenuItems[i].OfficeHint.Notes.AddStrings(mem_Notes.Lines);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.chk_CanSelectClick(Sender: TObject);
var
  i: Integer;
begin
  if FInternalItemChange then
    Exit;
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  if (i >= 0) then
    FPreviewMenuControl.AdvPreviewMenu.MenuItems[i].CanSelect := chk_CanSelect.Checked;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.chk_VisibleClick(Sender: TObject);
var
  i: Integer;
begin
  if FInternalItemChange then
    Exit;

  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  if (i >= 0) then
    FPreviewMenuControl.AdvPreviewMenu.MenuItems[i].Visible := chk_Visible.Checked;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.chk_EnabledClick(Sender: TObject);
var
  i: Integer;
begin
  if FInternalItemChange then
    Exit;
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  if (i >= 0) then
    FPreviewMenuControl.AdvPreviewMenu.MenuItems[i].Enabled := chk_Enabled.Checked;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.chk_SeparatorClick(Sender: TObject);
var
  i: Integer;
begin
  if FInternalItemChange then
    Exit;

  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  if (i >= 0) then
    FPreviewMenuControl.AdvPreviewMenu.MenuItems[i].Separator := chk_Separator.Checked;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.chk_HintShowHelpClick(Sender: TObject);
var
  i: Integer;
begin
  if FInternalItemChange then
    Exit;

  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  if (i >= 0) then
    FPreviewMenuControl.AdvPreviewMenu.MenuItems[i].OfficeHint.ShowHelp := chk_HintShowHelp.Checked;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.edt_CaptionKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  i: Integer;
begin
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  if (i >= 0) then
  begin
    FPreviewMenuControl.AdvPreviewMenu.MenuItems[i].Caption := edt_Caption.Text;
    UpdatePreview;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.edt_SubMenuCaptionKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  i: Integer;
begin
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  if (i >= 0) then
  begin
    FPreviewMenuControl.AdvPreviewMenu.MenuItems[i].SubMenuCaption := edt_SubMenuCaption.Text;
    //TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).InitializeAndUpdate;
    UpdatePreview;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.edt_ShortCutHintKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  i: Integer;
begin
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  if (i >= 0) then
    FPreviewMenuControl.AdvPreviewMenu.MenuItems[i].ShortCutHint := edt_ShortCutHint.Text;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.edt_ShortCutSubHintKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  i: Integer;
begin
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  if (i >= 0) then
    FPreviewMenuControl.AdvPreviewMenu.MenuItems[i].ShortCutSubItemsHint := edt_ShortCutSubHint.Text;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.edt_HintTitleKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  i: Integer;
begin
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  if (i >= 0) then
    FPreviewMenuControl.AdvPreviewMenu.MenuItems[i].OfficeHint.Title := edt_HintTitle.Text;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.btn_AddClick(Sender: TObject);
var
  i: Integer;
begin
  with FPreviewMenuControl.AdvPreviewMenu.MenuItems.Add do
  begin
    Caption := 'NewItem';
    SubMenuCaption := 'SubMenuCaption';
    i := Index;
  end;
  TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).InitializeAndUpdate;
  TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).ItemHot := i;
  TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectItem(i);
  TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).InvalidateRightFrame;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.btn_RemoveClick(Sender: TObject);
var
  i: Integer;
begin
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  if (i >= 0) then
  begin
    FPreviewMenuControl.AdvPreviewMenu.MenuItems.Delete(i);
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).InitializeAndUpdate;
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).ItemHot := i-1;
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectItem(i-1);
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.OnSubMenuItemClick(Sender: TObject; ItemIndex,
  SubMenuItemIndex: Integer);
var
  SubItems: TAdvPreviewSubMenuItems;
begin
  FInternalSubItemChange := True;
  if (SubMenuItemIndex < 0) then
  begin
    edt_Title.Text := '';
    edt_SubShortCutHint.Text := '';
    cmb_SubImageIndex.Text := '-1';
    spn_SubTag.Value := 0;
    mem_SubNotes.Lines.Clear;
    edt_SubHintTitle.Text := '';
    mem_SubHintNotes.Lines.Clear;
    chk_SubVisible.Checked := False;
    chk_SubEnabled.Checked := False;
    chk_SubSeparator.Checked := False;
    chk_SubShowHelp.Checked := False;
    if cmb_SubImageIndex.Enabled and (cmb_SubImageIndex.Items.Count > 0) then
      cmb_SubImageIndex.Text := InttoStr(-1);
    cmb_SubAction.Text := '';
    cmb_SubMenu.Text := '';
  end
  else
  begin
    SubItems := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).GetSubMenuItems;
    if Assigned(SubItems) and (SubMenuItemIndex < SubItems.Count) and (SubMenuItemIndex >= 0) then
    begin
      edt_Title.Text := SubItems.Items[SubMenuItemIndex].Title;
      edt_SubShortCutHint.Text := SubItems.Items[SubMenuItemIndex].ShortCutHint;
      cmb_SubImageIndex.Text := InttoStr(SubItems.Items[SubMenuItemIndex].ImageIndex);
      spn_SubTag.Value := SubItems.Items[SubMenuItemIndex].Tag;
      mem_SubNotes.Lines.Clear;
      mem_SubNotes.Lines.AddStrings(SubItems.Items[SubMenuItemIndex].Notes);
      edt_SubHintTitle.Text := SubItems.Items[SubMenuItemIndex].OfficeHint.Title;
      mem_SubHintNotes.Lines.Clear;
      mem_SubHintNotes.Lines.AddStrings(SubItems.Items[SubMenuItemIndex].OfficeHint.Notes);
      chk_SubVisible.Checked := SubItems.Items[SubMenuItemIndex].Visible;
      chk_SubEnabled.Checked := SubItems.Items[SubMenuItemIndex].Enabled;
      chk_SubSeparator.Checked := SubItems.Items[SubMenuItemIndex].Separator;
      chk_SubShowHelp.Checked := SubItems.Items[SubMenuItemIndex].OfficeHint.ShowHelp;
      if cmb_SubImageIndex.Enabled and (SubItems.Items[SubMenuItemIndex].ImageIndex < cmb_SubImageIndex.Items.Count) then
        cmb_SubImageIndex.ItemIndex := SubItems.Items[SubMenuItemIndex].ImageIndex;

      if Assigned(SubItems.Items[SubMenuItemIndex].Action) then
        cmb_SubAction.Text := SubItems.Items[SubMenuItemIndex].Action.Name
      else
        cmb_SubAction.Text := '';

      if Assigned(SubItems.Items[SubMenuItemIndex].SubMenu) then
        cmb_SubMenu.Text := SubItems.Items[SubMenuItemIndex].SubMenu.Name
      else
        cmb_SubMenu.Text := '';
    end
    else
    begin
      edt_Title.Text := '';
      edt_SubShortCutHint.Text := '';
      cmb_SubImageIndex.Text := '-1';
      spn_SubTag.Value := 0;
      mem_SubNotes.Lines.Clear;
      edt_SubHintTitle.Text := '';
      mem_SubHintNotes.Lines.Clear;
      chk_SubVisible.Checked := False;
      chk_SubEnabled.Checked := False;
      chk_SubSeparator.Checked := False;
      chk_SubShowHelp.Checked := False;
      if cmb_SubImageIndex.Enabled and (cmb_SubImageIndex.Items.Count > 0) then
        cmb_SubImageIndex.Text := InttoStr(-1);
      cmb_SubAction.Text := '';
      cmb_SubMenu.Text := '';  
    end;
  end;
  FInternalSubItemChange := False;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.edt_TitleKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  SubItems: TAdvPreviewSubMenuItems;
  i: Integer;
begin
  SubItems := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).GetSubMenuItems;
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedSubItem;
  //j := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  if (i >= 0) then
  begin
    SubItems.Items[i].Title := edt_Title.Text;
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).DrawSubMenuItem(i);
    UpdatePreview;
    {TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).InitializeAndUpdate;
    if (j >= 0) then
      TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).ItemHot := j;}
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.edt_SubShortCutHintKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  SubItems: TAdvPreviewSubMenuItems;
  i: Integer;
begin
  SubItems := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).GetSubMenuItems;
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedSubItem;
  if (i >= 0) then
    SubItems.Items[i].ShortCutHint := edt_SubShortCutHint.Text;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.cmb_SubImageIndexChange(Sender: TObject);
var
  SubItems: TAdvPreviewSubMenuItems;
  i: Integer;
begin
  if FInternalSubItemChange then
    Exit;

  SubItems := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).GetSubMenuItems;
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedSubItem;
  if (i >= 0) and Assigned(SubItems) and (cmb_SubImageIndex.ItemIndex >= 0) then
  begin
    SubItems.Items[i].ImageIndex := StrToInt(cmb_SubImageIndex.Items[cmb_SubImageIndex.ItemIndex]);
    UpdatePreview;
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).DrawSubMenuItem(i);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.spn_SubTagChange(Sender: TObject);
var
  SubItems: TAdvPreviewSubMenuItems;
  i: Integer;
begin
  if FInternalSubItemChange then
    Exit;

  SubItems := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).GetSubMenuItems;
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedSubItem;
  if (i >= 0) then
    SubItems.Items[i].Tag := spn_SubTag.Value;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.chk_SubVisibleClick(Sender: TObject);
var
  SubItems: TAdvPreviewSubMenuItems;
  i: Integer;
begin
  if FInternalSubItemChange then
    Exit;

  SubItems := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).GetSubMenuItems;
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedSubItem;
  if (i >= 0) then
    SubItems.Items[i].Visible := chk_SubVisible.Checked;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.chk_SubEnabledClick(Sender: TObject);
var
  SubItems: TAdvPreviewSubMenuItems;
  i: Integer;
begin
  if FInternalSubItemChange then
    Exit;

  SubItems := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).GetSubMenuItems;
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedSubItem;
  if (i >= 0) then
  begin
    SubItems.Items[i].Enabled := chk_SubEnabled.Checked;
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).DrawSubMenuItem(i);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.chk_SubSeparatorClick(Sender: TObject);
var
  SubItems: TAdvPreviewSubMenuItems;
  i: Integer;
begin
  if FInternalSubItemChange then
    Exit;

  SubItems := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).GetSubMenuItems;
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedSubItem;
  if (i >= 0) then
  begin
    SubItems.Items[i].Separator := chk_SubSeparator.Checked;
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).DrawSubMenuItem(i);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.mem_SubNotesChange(Sender: TObject);
var
  SubItems: TAdvPreviewSubMenuItems;
  i: Integer;
begin
  if FInternalSubItemChange then
    Exit;

  SubItems := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).GetSubMenuItems;
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedSubItem;
  if (i >= 0) then
  begin
    SubItems.Items[i].Notes.Clear;
    SubItems.Items[i].Notes.AddStrings(mem_SubNotes.Lines);
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).DrawSubMenuItem(i);
    //TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).InitializeAndUpdate;
    UpdatePreview;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.edt_SubHintTitleKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  SubItems: TAdvPreviewSubMenuItems;
  i: Integer;
begin
  if FInternalSubItemChange then
    Exit;

  SubItems := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).GetSubMenuItems;
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedSubItem;
  if (i >= 0) then
    SubItems.Items[i].OfficeHint.Title := edt_SubHintTitle.Text;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.mem_SubHintNotesChange(Sender: TObject);
var
  SubItems: TAdvPreviewSubMenuItems;
  i: Integer;
begin
  if FInternalSubItemChange then
    Exit;

  SubItems := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).GetSubMenuItems;
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedSubItem;
  if (i >= 0) then
  begin
    SubItems.Items[i].OfficeHint.Notes.Clear;
    SubItems.Items[i].OfficeHint.Notes.AddStrings(mem_SubHintNotes.Lines);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.chk_SubShowHelpClick(Sender: TObject);
var
  SubItems: TAdvPreviewSubMenuItems;
  i: Integer;
begin
  if FInternalSubItemChange then
    Exit;

  SubItems := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).GetSubMenuItems;
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedSubItem;
  if (i >= 0) then
    SubItems.Items[i].OfficeHint.ShowHelp := chk_SubShowHelp.Checked;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.btn_SubItemAddClick(Sender: TObject);
var
  i, j: Integer;
begin
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  if (i >= 0) then
  begin
    with FPreviewMenuControl.AdvPreviewMenu.MenuItems.Items[i].SubItems.Add do
    begin
      Title := 'NewItem';
      j := Index;
    end;
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).InitializeAndUpdate;
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).ItemHot := i;
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SetSelectedSubItem(j);
    FPreviewMenuControl.AdvPreviewMenuPanel.Invalidate;

    btn_AddDefault.Enabled := False;
    btn_RemoveDefault.Enabled := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.btn_SubItemRemoveClick(Sender: TObject);
var
  i, j: Integer;
begin
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  j := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedSubItem;
  if (i >= 0) and (j >= 0) and (j < FPreviewMenuControl.AdvPreviewMenu.MenuItems.Items[i].SubItems.Count) then
  begin
    FPreviewMenuControl.AdvPreviewMenu.MenuItems.Items[i].SubItems.Delete(j);
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).InitializeAndUpdate;
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).ItemHot := i;
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SetSelectedSubItem(j-1);
    FPreviewMenuControl.AdvPreviewMenuPanel.Invalidate;

    btn_AddDefault.Enabled := (FPreviewMenuControl.AdvPreviewMenu.MenuItems.Items[i].SubItems.Count = 0);
    btn_RemoveDefault.Enabled := (FPreviewMenuControl.AdvPreviewMenu.MenuItems.Items[i].SubItems.Count = 0);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.UpdatePreview;
var
  i: Integer;
begin
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).InitializeAndUpdate;
  if (i >= 0) then
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).ItemHot := i;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.btn_AddDefaultClick(Sender: TObject);
var
  i, j: Integer;
begin
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  if (i >= 0) then
  begin
    with FPreviewMenuControl.AdvPreviewMenu.SubMenuItems.Add do
    begin
      Title := 'NewItem';
      j := Index;
    end;
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).InitializeAndUpdate;
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).ItemHot := i;
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SetSelectedSubItem(j);
    FPreviewMenuControl.AdvPreviewMenuPanel.Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.btn_RemoveDefaultClick(Sender: TObject);
var
  i, j: Integer;
begin
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  j := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedSubItem;
  if (i >= 0) and (j >= 0) and (j < FPreviewMenuControl.AdvPreviewMenu.SubMenuItems.Count) then
  begin
    FPreviewMenuControl.AdvPreviewMenu.SubMenuItems.Delete(j);
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).InitializeAndUpdate;
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).ItemHot := i;
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SetSelectedSubItem(j-1);
    FPreviewMenuControl.AdvPreviewMenuPanel.Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.btn_PictureClick(Sender: TObject);
var
  i: Integer;
begin
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  if (i >= 0) then
  begin
    if Opendialog.Execute then
    begin
      FPreviewMenuControl.AdvPreviewMenu.MenuItems[i].Picture.LoadFromFile(Opendialog.FileName);
      UpdatePreview;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.btn_DisabledPicClick(Sender: TObject);
var
  i: Integer;
begin
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  if (i >= 0) then
  begin
    if Opendialog.Execute then
    begin
      FPreviewMenuControl.AdvPreviewMenu.MenuItems[i].DisabledPicture.LoadFromFile(Opendialog.FileName);
      UpdatePreview;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.btn_SubPictureClick(Sender: TObject);
var
  SubItems: TAdvPreviewSubMenuItems;
  i: Integer;
begin
  SubItems := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).GetSubMenuItems;
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedSubItem;
  if (i >= 0) and Assigned(SubItems) then
  begin
    if Opendialog.Execute then
    begin
      SubItems.Items[i].Picture.LoadFromFile(Opendialog.FileName);
      UpdatePreview;
      TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).DrawSubMenuItem(i);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.btn_SubDisabledPicClick(Sender: TObject);
var
  SubItems: TAdvPreviewSubMenuItems;
  i: Integer;
begin
  SubItems := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).GetSubMenuItems;
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedSubItem;
  if (i >= 0) and Assigned(SubItems) then
  begin
    if Opendialog.Execute then
    begin
      SubItems.Items[i].DisabledPicture.LoadFromFile(Opendialog.FileName);
      UpdatePreview;
      TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).DrawSubMenuItem(i);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.cmb_SubImageIndexDrawItem(
  Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  ImgList: TCustomImageList;
  ACanvas: TCanvas;
  X, Y: Integer;
begin
  ACanvas := cmb_SubImageIndex.Canvas;
  ACanvas.FillRect (Rect);
  ImgList := FPreviewMenuControl.AdvPreviewMenu.SubMenuImages;
  X := Rect.Left + 2;

  if Assigned(ImgList) then
  begin
    if (cmb_SubImageIndex.Items[Index] <> '-1') then
      ImgList.Draw (ACanvas, X, Rect.Top + 2, StrToInt(cmb_SubImageIndex.Items[Index]));
    Inc (X, ImgList.Width);
  end;

  Y := Rect.Top + 1 + (cmb_SubImageIndex.ItemHeight - ACanvas.TextHeight('None')) div 2;
  if (cmb_SubImageIndex.Items[Index] = '-1') then
    ACanvas.TextOut (X + 3, Y, '(None)')
  else
    ACanvas.TextOut (X + 3, Y, cmb_SubImageIndex.Items[Index]);
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.Loaded;
begin
  inherited;

end;

//------------------------------------------------------------------------------

function TPreviewMenuEditor.ShowModal: Integer;
var
  i: Integer;
begin
  // Manu Images
  if Assigned(FPreviewMenuControl.AdvPreviewMenu.MenuImages) then
  begin
    cmb_ImageIndex.Enabled := True;
    cmb_ImageIndex.ItemHeight := FPreviewMenuControl.AdvPreviewMenu.MenuImages.Height;
    cmb_ImageIndex.Clear;
    for i := 0 to FPreviewMenuControl.AdvPreviewMenu.MenuImages.Count-1 do
    begin
      cmb_ImageIndex.Items.Add(InttoStr(i));
    end;
    cmb_ImageIndex.Items.Add(InttoStr(-1));
  end
  else
    cmb_ImageIndex.Enabled := False;
  
  // SubMenu Images
  if Assigned(FPreviewMenuControl.AdvPreviewMenu.SubMenuImages) then
  begin
    cmb_SubImageIndex.Enabled := True;
    cmb_SubImageIndex.ItemHeight := FPreviewMenuControl.AdvPreviewMenu.SubMenuImages.Height;
    cmb_SubImageIndex.Clear;
    for i:= 0 to FPreviewMenuControl.AdvPreviewMenu.SubMenuImages.Count -1 do
    begin
      cmb_SubImageIndex.Items.Add(InttoStr(i));
    end;
    cmb_SubImageIndex.Items.Add(InttoStr(-1));
  end
  else
    cmb_SubImageIndex.Enabled := False;

  if (FPreviewMenuControl.AdvPreviewMenu.MenuItems.Count > 0) then
  begin
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).ItemHot := 0;
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SetSelectedItem(0);
  end;
  Result := inherited ShowModal;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.cmb_ImageIndexDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  ImgList: TCustomImageList;
  ACanvas: TCanvas;
  X, Y: Integer;
begin
  ACanvas := cmb_ImageIndex.Canvas;
  ACanvas.FillRect (Rect);
  ImgList := FPreviewMenuControl.AdvPreviewMenu.MenuImages;
  X := Rect.Left + 2;

  if Assigned(ImgList) then
  begin
    if (cmb_ImageIndex.Items[Index] <> '-1') then
      ImgList.Draw (ACanvas, X, Rect.Top + 2, StrToInt(cmb_ImageIndex.Items[Index]));
    Inc (X, ImgList.Width);
  end;

  Y := Rect.Top + 1 + (cmb_ImageIndex.ItemHeight - ACanvas.TextHeight('None')) div 2;
  if (cmb_ImageIndex.Items[Index] = '-1') then
    ACanvas.TextOut(X + 3, Y, '(None)')
  else
    ACanvas.TextOut(X + 3, Y, cmb_ImageIndex.Items[Index]);
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.cmb_ActionChange(Sender: TObject);
var
  i: Integer;
begin
  if FInternalItemChange then
    Exit;

  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  if (i >= 0) then
  begin
    if (cmb_Action.ItemIndex >= 0) then
      FPreviewMenuControl.AdvPreviewMenu.MenuItems[i].Action := TBasicAction(cmb_Action.Items.Objects[cmb_Action.ItemIndex])
    else
      FPreviewMenuControl.AdvPreviewMenu.MenuItems[i].Action := nil;
    UpdatePreview;
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).DrawItem(i);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.cmb_SubActionChange(Sender: TObject);
var
  SubItems: TAdvPreviewSubMenuItems;
  i: Integer;
begin
  if FInternalSubItemChange then
    Exit;

  SubItems := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).GetSubMenuItems;
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedSubItem;
  if (i >= 0) and Assigned(SubItems) then
  begin
    if (cmb_SubAction.ItemIndex >= 0) then
      SubItems.Items[i].Action := TBasicAction(cmb_SubAction.Items.Objects[cmb_SubAction.ItemIndex])
    else
      SubItems.Items[i].Action := nil;
    UpdatePreview;
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).DrawSubMenuItem(i);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuEditor.cmb_SubMenuChange(Sender: TObject);
var
  SubItems: TAdvPreviewSubMenuItems;
  i: Integer;
begin
  if FInternalSubItemChange then
    Exit;

  SubItems := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).GetSubMenuItems;
  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedSubItem;
  if (i >= 0) and Assigned(SubItems) then
  begin
    if (cmb_SubMenu.ItemIndex >= 0) then
      SubItems.Items[i].SubMenu := TPopupMenu(cmb_SubMenu.Items.Objects[cmb_SubMenu.ItemIndex])
    else
      SubItems.Items[i].SubMenu := nil;
    UpdatePreview;
    TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).DrawSubMenuItem(i);
  end;
end;

procedure TPreviewMenuEditor.spn_SubMenuHeightChange(Sender: TObject);
var
  i: Integer;
begin
  if FInternalItemChange then
    Exit;

  if (spn_SubMenuHeight.Value < 0) then
    spn_SubMenuHeight.Value := 0;

  if (spn_SubMenuHeight.Value = 1) then
    spn_SubMenuHeight.Value := 12;

  if (spn_SubMenuHeight.Value <= 11) then
    spn_SubMenuHeight.Value := 0;

  i := TProPreviewMenuPanel(FPreviewMenuControl.AdvPreviewMenuPanel).SelectedItem;
  if (i >= 0) then
  begin
    FPreviewMenuControl.AdvPreviewMenu.MenuItems[i].SubMenuItemHeight := spn_SubMenuHeight.Value;
    UpdatePreview;
  end;
end;

end.
