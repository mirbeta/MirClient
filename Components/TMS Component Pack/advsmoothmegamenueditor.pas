{***************************************************************************}
{ TAdvSmoothMegaMenuEditor component                                        }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2012                                               }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvSmoothMegaMenuEditor;

interface

{$I TMSDEFS.INC}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, GdipMenu, GdipFill, AdvSmoothfillEditor, StdCtrls,
  Buttons, ImgList, AdvSmoothSpin, AdvSmoothHTMLEditor, Mask, Menus,
  AdvGDIP, Math
  ;

type
  TCustomPaintBox = class(TPaintBox)
  private
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
  end;

  TMenuNodeType = (ntRoot, ntItem, ntSection, ntTopLayerItem);

  TMenuNode = class
    root: TGDIPMenu;
    item: TGDIPMenuSectionItem;
    section: TGDIPMenuSection;
    toplayeritem: TGDIPMenuTopLayerItem;
  end;

  TAdvSmoothMegaMenuEditorForm = class(TForm)
    Panel1: TPanel;
    TreeView1: TTreeView;
    Panel2: TPanel;
    Panel3: TPanel;
    ScrollBox1: TScrollBox;
    Panel4: TPanel;
    Button1: TButton;
    Button4: TButton;
    pnlSectionItem: TPanel;
    pnlTop: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    ImageList1: TImageList;
    pnlEmpty: TPanel;
    FontDialog1: TFontDialog;
    PageControl3: TPageControl;
    TabSheet5: TTabSheet;
    CheckBox3: TCheckBox;
    Label42: TStaticText;
    Label43: TStaticText;
    SpinEdit20: TAdvSmoothSpinEdit;
    SpinEdit21: TAdvSmoothSpinEdit;
    Label44: TStaticText;
    SpinEdit22: TAdvSmoothSpinEdit;
    SpinEdit23: TAdvSmoothSpinEdit;
    Label45: TStaticText;
    Label46: TStaticText;
    PaintBox12: TPaintBox;
    Button18: TButton;
    ComboBox4: TComboBox;
    Label47: TStaticText;
    TabSheet6: TTabSheet;
    Label48: TStaticText;
    Label49: TStaticText;
    ComboBox5: TComboBox;
    Label50: TStaticText;
    Button19: TButton;
    AdvSmoothSpinEdit1: TAdvSmoothSpinEdit;
    AdvSmoothSpinEdit2: TAdvSmoothSpinEdit;
    Label51: TStaticText;
    Label52: TStaticText;
    PageControl4: TPageControl;
    TabSheet7: TTabSheet;
    Label53: TStaticText;
    AdvSmoothSpinEdit3: TAdvSmoothSpinEdit;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    TabSheet8: TTabSheet;
    ComboBox6: TComboBox;
    Label54: TStaticText;
    ComboBox7: TComboBox;
    Label55: TStaticText;
    Label56: TStaticText;
    AdvSmoothSpinEdit4: TAdvSmoothSpinEdit;
    TabSheet9: TTabSheet;
    Label57: TStaticText;
    ComboBox8: TComboBox;
    Label12: TStaticText;
    Button20: TButton;
    Label58: TStaticText;
    Button21: TButton;
    Button22: TButton;
    Label59: TStaticText;
    ComboBox9: TComboBox;
    AdvSmoothSpinEdit5: TAdvSmoothSpinEdit;
    AdvSmoothSpinEdit6: TAdvSmoothSpinEdit;
    Label61: TStaticText;
    Label62: TStaticText;
    AdvSmoothSpinEdit7: TAdvSmoothSpinEdit;
    AdvSmoothSpinEdit8: TAdvSmoothSpinEdit;
    Label64: TStaticText;
    Label65: TStaticText;
    Label66: TStaticText;
    Label67: TStaticText;
    Label63: TStaticText;
    Label68: TStaticText;
    ComboBox10: TComboBox;
    AdvSmoothSpinEdit9: TAdvSmoothSpinEdit;
    AdvSmoothSpinEdit10: TAdvSmoothSpinEdit;
    Label69: TStaticText;
    ColorDialog1: TColorDialog;
    ComboBox11: TComboBox;
    ComboBox12: TComboBox;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    Label75: TStaticText;
    Edit2: TEdit;
    StaticText1: TStaticText;
    AdvSmoothSpinEdit12: TAdvSmoothSpinEdit;
    StaticText2: TStaticText;
    StaticText4: TStaticText;
    ComboBox13: TComboBox;
    StaticText5: TStaticText;
    AdvSmoothSpinEdit13: TAdvSmoothSpinEdit;
    StaticText7: TStaticText;
    CheckBox8: TCheckBox;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    pnlRoot: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    PaintBox8: TPaintBox;
    PaintBox9: TPaintBox;
    PaintBox13: TPaintBox;
    PaintBox14: TPaintBox;
    Label7: TStaticText;
    Label8: TStaticText;
    Label9: TStaticText;
    Label22: TStaticText;
    Label23: TStaticText;
    Label24: TStaticText;
    Label71: TStaticText;
    Label72: TStaticText;
    Label73: TStaticText;
    Label74: TStaticText;
    CheckBox2: TCheckBox;
    Button9: TButton;
    SpinEdit1: TAdvSmoothSpinEdit;
    Button10: TButton;
    ComboBox1: TComboBox;
    SpinEdit8: TAdvSmoothSpinEdit;
    SpinEdit9: TAdvSmoothSpinEdit;
    AdvSmoothSpinEdit11: TAdvSmoothSpinEdit;
    Button25: TButton;
    Button26: TButton;
    TabSheet10: TTabSheet;
    Label25: TStaticText;
    Label18: TStaticText;
    Label17: TStaticText;
    Label19: TStaticText;
    Label21: TStaticText;
    Label20: TStaticText;
    CheckBox1: TCheckBox;
    ComboBox2: TComboBox;
    SpinEdit4: TAdvSmoothSpinEdit;
    SpinEdit5: TAdvSmoothSpinEdit;
    SpinEdit6: TAdvSmoothSpinEdit;
    SpinEdit7: TAdvSmoothSpinEdit;
    TabSheet2: TTabSheet;
    PaintBox6: TPaintBox;
    PaintBox7: TPaintBox;
    PaintBox4: TPaintBox;
    PaintBox5: TPaintBox;
    PaintBox3: TPaintBox;
    PaintBox2: TPaintBox;
    Label1: TStaticText;
    Label2: TStaticText;
    Label4: TStaticText;
    Label3: TStaticText;
    Label6: TStaticText;
    Label5: TStaticText;
    Label10: TStaticText;
    Label11: TStaticText;
    Label13: TStaticText;
    Label14: TStaticText;
    Label15: TStaticText;
    Label16: TStaticText;
    Label70: TStaticText;
    Button8: TButton;
    Button7: TButton;
    Button6: TButton;
    Button5: TButton;
    Button3: TButton;
    Button2: TButton;
    SpinEdit2: TAdvSmoothSpinEdit;
    SpinEdit3: TAdvSmoothSpinEdit;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button24: TButton;
    CheckBox7: TCheckBox;
    StaticText6: TStaticText;
    Button27: TButton;
    pnlSection: TPanel;
    PageControl2: TPageControl;
    TabSheet3: TTabSheet;
    PaintBox11: TPaintBox;
    PaintBox10: TPaintBox;
    Label33: TStaticText;
    Label34: TStaticText;
    Label35: TStaticText;
    Label26: TStaticText;
    Label27: TStaticText;
    Label28: TStaticText;
    Label29: TStaticText;
    Label32: TStaticText;
    Label31: TStaticText;
    Label30: TStaticText;
    SpinEdit13: TAdvSmoothSpinEdit;
    SpinEdit14: TAdvSmoothSpinEdit;
    Button17: TButton;
    Edit1: TEdit;
    Button15: TButton;
    ComboBox3: TComboBox;
    SpinEdit11: TAdvSmoothSpinEdit;
    SpinEdit10: TAdvSmoothSpinEdit;
    SpinEdit12: TAdvSmoothSpinEdit;
    Button16: TButton;
    StaticText3: TStaticText;
    TabSheet4: TTabSheet;
    Label36: TStaticText;
    Label37: TStaticText;
    Label38: TStaticText;
    Label39: TStaticText;
    Label40: TStaticText;
    Label41: TStaticText;
    SpinEdit15: TAdvSmoothSpinEdit;
    SpinEdit16: TAdvSmoothSpinEdit;
    SpinEdit17: TAdvSmoothSpinEdit;
    SpinEdit18: TAdvSmoothSpinEdit;
    SpinEdit19: TAdvSmoothSpinEdit;
    CheckBox9: TCheckBox;
    procedure PaintBox1Paint(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure PaintBox2Paint(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure PaintBox3Paint(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure PaintBox5Paint(Sender: TObject);
    procedure PaintBox4Paint(Sender: TObject);
    procedure PaintBox7Paint(Sender: TObject);
    procedure PaintBox6Paint(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure PaintBox8Paint(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure PaintBox9Paint(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure SpinEdit3Change(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure SpinEdit4Change(Sender: TObject);
    procedure SpinEdit5Change(Sender: TObject);
    procedure SpinEdit6Change(Sender: TObject);
    procedure SpinEdit7Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure SpinEdit8Change(Sender: TObject);
    procedure SpinEdit9Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure SpinEdit11Change(Sender: TObject);
    procedure SpinEdit10Change(Sender: TObject);
    procedure SpinEdit12Change(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure PaintBox10Paint(Sender: TObject);
    procedure SpinEdit14Change(Sender: TObject);
    procedure SpinEdit13Change(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure PaintBox11Paint(Sender: TObject);
    procedure SpinEdit15Change(Sender: TObject);
    procedure SpinEdit16Change(Sender: TObject);
    procedure SpinEdit17Change(Sender: TObject);
    procedure SpinEdit18Change(Sender: TObject);
    procedure SpinEdit19Change(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure SpinEdit20Change(Sender: TObject);
    procedure SpinEdit21Change(Sender: TObject);
    procedure SpinEdit23Change(Sender: TObject);
    procedure SpinEdit22Change(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure PaintBox12Paint(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure ComboBox5Change(Sender: TObject);
    procedure Button19Click(Sender: TObject);
    procedure AdvSmoothSpinEdit1Change(Sender: TObject);
    procedure AdvSmoothSpinEdit2Change(Sender: TObject);
    procedure AdvSmoothSpinEdit3Change(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure ComboBox6Change(Sender: TObject);
    procedure ComboBox7Change(Sender: TObject);
    procedure AdvSmoothSpinEdit4Change(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button20Click(Sender: TObject);
    procedure Button21Click(Sender: TObject);
    procedure Button22Click(Sender: TObject);
    procedure ComboBox9Change(Sender: TObject);
    procedure AdvSmoothSpinEdit5Change(Sender: TObject);
    procedure AdvSmoothSpinEdit6Change(Sender: TObject);
    procedure AdvSmoothSpinEdit8Change(Sender: TObject);
    procedure AdvSmoothSpinEdit7Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboBox10Change(Sender: TObject);
    procedure AdvSmoothSpinEdit9Change(Sender: TObject);
    procedure AdvSmoothSpinEdit10Change(Sender: TObject);
    procedure ComboBox11Change(Sender: TObject);
    procedure ComboBox12Change(Sender: TObject);
    procedure Button24Click(Sender: TObject);
    procedure AdvSmoothSpinEdit11Change(Sender: TObject);
    procedure Button25Click(Sender: TObject);
    procedure Button26Click(Sender: TObject);
    procedure PaintBox13Paint(Sender: TObject);
    procedure PaintBox14Paint(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure ComboBox8Change(Sender: TObject);
    procedure AdvSmoothSpinEdit12Change(Sender: TObject);
    procedure ComboBox13Change(Sender: TObject);
    procedure Button27Click(Sender: TObject);
    procedure AdvSmoothSpinEdit13Change(Sender: TObject);
    procedure Button23Click(Sender: TObject);
    procedure TreeView1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TreeView1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure CheckBox8Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
  private
    { Private declarations }
    PaintBox1: TCustomPaintBox;
    tl: TGDIPMenuTopLayerItem;
    TLLeft: integer;
    TLTop: integer;
    FMenuPreview: TGDIPMenu;
    FSelectedNode: TTreeNode;
    FRootNode: TTreeNode;
    procedure SetSelectedNode(const Value: TTreeNode);
    procedure SetRootNode(const Value: TTreeNode);
  protected
    procedure MenuChanged(Sender: TObject);
    procedure MenuItemCheckChanged(Sender: TObject; item: TGDIPMenuSectionItem);
    property SelectedNode: TTreeNode read FSelectedNode write SetSelectedNode;
    property RootNode: TTreeNode read FRootNode write SetRootNode;
  public
    { Public declarations }
    procedure Init(Create: Boolean);
    procedure CalculateSize;
    procedure UpdateMenu;
    procedure SaveChanges;
    procedure CorrectTop(lbl: TStaticText);
    procedure ClearTreeView;
    procedure InitTreeView(Expand, SelectRoot: Boolean; RootIndex: integer = - 1; RootTopIndex: integer = -1;
      RootItemIndex: integer = -1; RootSubItemIndex: integer = -1);
    procedure HidePanels;
    procedure StartFillEditor(f: TGDIPFill);
    procedure StartHTMLEditor(var str: String);
    procedure PaintFill(pb: TPaintBox; f: TGDIPFill);
    procedure SelectItemTreeView(si: TGDIPMenuSectionItem);
    procedure SelectTopLayerTreeView(tl: TGDIPMenuTopLayerItem);
    procedure FillMenuProperties(m: TGDIPMenu; Hidepnl: Boolean);
    procedure DisableButtons;
    procedure FillSectionProperties(s: TGDIPMenuSection; Hidepnl: Boolean);
    procedure FillSectionItemProperties(si: TGDIPMenuSectionItem; Hidepnl: Boolean);
    procedure FillTopLayerItemProperties(tl: TGDIPMenuTopLayerItem; Hidepnl: Boolean);
    function GetMenuNodeType(m: TMenuNode): TMenuNodeType;
    property MenuPreview: TGDIPMenu read FMenuPreview write FMenuPreview;
  end;

  TAdvSmoothMegaMenuEditorDialog = class(TCommonDialog)
  private
    FForm: TAdvSmoothMegaMenuEditorForm;
    FCaption: string;
    FMenu: TGDIPMenu;
    procedure SetMenu(const Value: TGDIPMenu);
  protected
  public
    function Execute: Boolean; override;
    property Form: TAdvSmoothMegaMenuEditorForm read FForm;
    property Menu: TGDIPMenu read FMenu write SetMenu;
  published
    property Caption: string read FCaption write FCaption;
  end;

  TSaveMode = (mCancelled, mSaved, mNotSaved);

var
  AdvSmoothMegaMenuEditorForm: TAdvSmoothMegaMenuEditorForm;
  Mode: TSaveMode;
  m: TGDIPMenu;
  DoTLDrag: Boolean;
  DoRefresh: Boolean;
  FMenuIsUpdated, FAllowUpdate: Boolean;

implementation

{$R *.dfm}

{ TAdvSmoothMegaMenuEditorDialog }

function TAdvSmoothMegaMenuEditorDialog.Execute: Boolean;
begin
  FForm := TAdvSmoothMegaMenuEditorForm.Create(Application);

  if not Assigned(FMenu) then
  begin
    raise Exception.Create('The dialog does not have a TGDIPMenu class assigned.');
    Result := False;
    Exit;
  end;

  if FCaption <> '' then
    Form.Caption := FCaption;

  try
    FForm.MenuPreview := FMenu;
    FForm.Init(true);
    Result := FForm.ShowModal = mrOK;
  finally
    FForm.Free;
  end;
end;

procedure TAdvSmoothMegaMenuEditorDialog.SetMenu(const Value: TGDIPMenu);
begin
  FMenu := Value;
end;

{ TAdvSmoothMegaMenuEditorForm }

procedure TAdvSmoothMegaMenuEditorForm.AdvSmoothSpinEdit10Change(
  Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
    TMenuNode(SelectedNode.Data).item.TextLeft := AdvSmoothSpinEdit10.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.AdvSmoothSpinEdit11Change(
  Sender: TObject);
begin
  if Assigned(m) then
    m.ItemAppearance.ShadowOffset := AdvSmoothSpinEdit11.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.AdvSmoothSpinEdit12Change(
  Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
    TMenuNode(SelectedNode.Data).item.Tag := AdvSmoothSpinEdit12.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.AdvSmoothSpinEdit13Change(
  Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
    TMenuNode(SelectedNode.Data).item.ImageIndex := AdvSmoothSpinEdit13.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.AdvSmoothSpinEdit1Change(
  Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).toplayeritem) then
    TMenuNode(SelectedNode.Data).toplayeritem.HTMLTextLeft := AdvSmoothSpinEdit1.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.AdvSmoothSpinEdit2Change(
  Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).toplayeritem) then
    TMenuNode(SelectedNode.Data).toplayeritem.HTMLTextTop := AdvSmoothSpinEdit2.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.AdvSmoothSpinEdit3Change(
  Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
    TMenuNode(SelectedNode.Data).item.GroupIndex := AdvSmoothSpinEdit3.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.AdvSmoothSpinEdit4Change(
  Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
    TMenuNode(SelectedNode.Data).item.ControlIndent := AdvSmoothSpinEdit4.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.AdvSmoothSpinEdit5Change(
  Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
    TMenuNode(SelectedNode.Data).item.DetailTextLeft := AdvSmoothSpinEdit5.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.AdvSmoothSpinEdit6Change(
  Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
    TMenuNode(SelectedNode.Data).item.DetailTextTop := AdvSmoothSpinEdit6.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.AdvSmoothSpinEdit7Change(
  Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
    TMenuNode(SelectedNode.Data).item.SelectedTopLayerItem := AdvSmoothSpinEdit7.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.AdvSmoothSpinEdit8Change(
  Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
    TMenuNode(SelectedNode.Data).item.HoverTopLayerItem := AdvSmoothSpinEdit8.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.AdvSmoothSpinEdit9Change(
  Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
    TMenuNode(SelectedNode.Data).item.TextLeft := AdvSmoothSpinEdit9.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button10Click(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).root) then
  begin
    StartFillEditor(TMenuNode(FSelectedNode.Data).root.ContentFill);
    PaintBox9.Invalidate;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button11Click(Sender: TObject);
begin
  FontDialog1.Font.Assign(m.ItemAppearance.Font);
  if FontDialog1.Execute then
  begin
    m.ItemAppearance.Font.Assign(FontDialog1.Font);
    Label13.font.assign(m.ItemAppearance.Font);
    CorrectTop(Label13);
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button12Click(Sender: TObject);
begin
  FontDialog1.Font.Assign(m.ItemAppearance.FontSelected);
  if FontDialog1.Execute then
  begin
    m.ItemAppearance.FontSelected.Assign(FontDialog1.Font);
    Label14.font.assign(m.ItemAppearance.FontSelected);
    CorrectTop(Label14);
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button13Click(Sender: TObject);
begin
  FontDialog1.Font.Assign(m.ItemAppearance.FontHover);
  if FontDialog1.Execute then
  begin
    m.ItemAppearance.FontHover.Assign(FontDialog1.Font);
    Label15.font.assign(m.ItemAppearance.FontHover);
    CorrectTop(Label15);
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button14Click(Sender: TObject);
begin
  FontDialog1.Font.Assign(m.ItemAppearance.FontDisabled);
  if FontDialog1.Execute then
  begin
    m.ItemAppearance.FontDisabled.Assign(FontDialog1.Font);
    Label16.font.assign(m.ItemAppearance.FontDisabled);
    CorrectTop(Label16);
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button15Click(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).section) then
  begin
    FontDialog1.Font.Assign(TMenuNode(SelectedNode.Data).section.CaptionFont);
    if FontDialog1.Execute then
    begin
      TMenuNode(SelectedNode.Data).section.CaptionFont.Assign(FontDialog1.Font);
      Label27.font.assign(TMenuNode(SelectedNode.Data).section.CaptionFont);
      CorrectTop(Label27);
    end;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button16Click(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).section) then
  begin
    StartFillEditor(TMenuNode(SelectedNode.Data).section.CaptionFill);
    PaintBox10.Invalidate;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button17Click(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).section) then
  begin
    StartFillEditor(TMenuNode(SelectedNode.Data).section.BackGroundFill);
    PaintBox11.Invalidate;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button18Click(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).toplayeritem) then
  begin
    StartFillEditor(TMenuNode(SelectedNode.Data).toplayeritem.Fill);
    PaintBox12.Invalidate;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button19Click(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).toplayeritem) then
  begin
    FontDialog1.Font.Assign(TMenuNode(SelectedNode.Data).toplayeritem.HTMLTextFont);
    if FontDialog1.Execute then
    begin
      TMenuNode(SelectedNode.Data).toplayeritem.HTMLTextFont.Assign(FontDialog1.Font);
      Label50.font.assign(TMenuNode(SelectedNode.Data).toplayeritem.HTMLTextFont);
      CorrectTop(Label50);
    end;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button1Click(Sender: TObject);
begin
  Mode := mSaved;
  SaveChanges;
  Self.Close;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button20Click(Sender: TObject);
var
  s: String;
begin
  if Assigned(TMenuNode(SelectedNode.Data).toplayeritem) then
  begin
    s := TMenuNode(SelectedNode.Data).toplayeritem.HTMLText;
    StartHTMLEditor(s);
    TMenuNode(SelectedNode.Data).toplayeritem.HTMLText := s;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button21Click(Sender: TObject);
var
  s: String;
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
  begin
    s := TMenuNode(SelectedNode.Data).item.Text;
    StartHTMLEditor(s);
    TMenuNode(SelectedNode.Data).item.Text := s;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button22Click(Sender: TObject);
var
  s: String;
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
  begin
    s := TMenuNode(SelectedNode.Data).item.DetailText;
    StartHTMLEditor(s);
    TMenuNode(SelectedNode.Data).item.DetailText := s;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button23Click(Sender: TObject);
begin
//
end;

procedure TAdvSmoothMegaMenuEditorForm.Button24Click(Sender: TObject);
begin
  FontDialog1.Font.Assign(m.ItemAppearance.DetailFont);
  if FontDialog1.Execute then
  begin
    m.ItemAppearance.DetailFont.Assign(FontDialog1.Font);
    Label70.font.assign(m.ItemAppearance.DetailFont);
    CorrectTop(Label70);
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button25Click(Sender: TObject);
begin
  ColorDialog1.Color := m.ItemAppearance.ShadowColor;
  if ColorDialog1.Execute then
  begin
    m.ItemAppearance.ShadowColor := ColorDialog1.Color;
    PaintBox13.Color := ColorDialog1.Color;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button26Click(Sender: TObject);
begin
  ColorDialog1.Color := m.ItemAppearance.URLColor;
  if ColorDialog1.Execute then
  begin
    m.ItemAppearance.URLColor := ColorDialog1.Color;
    PaintBox14.Color := ColorDialog1.Color;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button27Click(Sender: TObject);
begin
  FontDialog1.Font.Assign(m.ItemAppearance.ShortCutFont);
  if FontDialog1.Execute then
  begin
    m.ItemAppearance.ShortCutFont.Assign(FontDialog1.Font);
    StaticText6.font.assign(m.ItemAppearance.ShortCutFont);
    CorrectTop(StaticText6);
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button2Click(Sender: TObject);
begin
  StartFillEditor(m.ItemAppearance.SeparatorFill);
  PaintBox2.Invalidate;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button3Click(Sender: TObject);
begin
  StartFillEditor(m.ItemAppearance.BreakFill);
  PaintBox3.Invalidate;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button4Click(Sender: TObject);
begin
  Mode := mCancelled;
  Self.Close;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button5Click(Sender: TObject);
begin
  StartFillEditor(m.ItemAppearance.Fill);
  PaintBox5.Invalidate;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button6Click(Sender: TObject);
begin
  StartFillEditor(m.ItemAppearance.FillSelected);
  PaintBox4.Invalidate;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button7Click(Sender: TObject);
begin
  StartFillEditor(m.ItemAppearance.FillHover);
  PaintBox7.Invalidate;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button8Click(Sender: TObject);
begin
  StartFillEditor(m.ItemAppearance.FillDisabled);
  PaintBox6.Invalidate;
end;

procedure TAdvSmoothMegaMenuEditorForm.Button9Click(Sender: TObject);
begin
  StartFillEditor(m.TearOffFill);
  PaintBox8.Invalidate;
end;

procedure TAdvSmoothMegaMenuEditorForm.CalculateSize;
var
  i: integer;
  maxh, maxw: integer;
begin
  maxh := 0;
  maxw := 0;
  with m do
  begin
    if m.AutoSectionSize then
    begin
      m.DoAutoSize;
    end;

    for I := 0 to m.Sections.Count - 1 do
    begin
      case m.SectionLayout of
        slHorizontal:
        begin
          if m.Sections[I].GetHeight > maxh then
            maxh := Round(m.Sections[I].GetHeight);

           maxw := maxw + Round(m.Sections[I].GetWidth);
        end;
        slVertical:
        begin
          if m.Sections[I].GetWidth > maxw then
            maxw := Round(m.Sections[I].GetWidth);

          maxh := maxh + Round(m.Sections[I].GetHeight);
        end;
      end;
    end;

    PaintBox1.Height := maxh + m.SectionMargin.Top + m.SectionMargin.Bottom;
    PaintBox1.Width := maxw + m.SectionMargin.Left + m.SectionMargin.Right;
    if m.ContentFill.BorderColor <> clNone then
    begin
      PaintBox1.Height := PaintBox1.Height + (m.ContentFill.BorderWidth * 2);
      PaintBox1.Width := PaintBox1.Width + (m.ContentFill.BorderWidth * 2);
    end;

    if m.ContentFill.ShadowColor <> clNone then
    begin
      PaintBox1.Height := PaintBox1.Height + m.ContentFill.ShadowOffset;
      PaintBox1.Width := PaintBox1.Width + m.ContentFill.ShadowOffset;
    end;

    if (m.TearOffSize > 0) and m.TearOff then
    begin
      PaintBox1.Height := PaintBox1.Height + m.TearOffSize;
    end;

    PaintBox1.Height := PaintBox1.Height + 1;
    PaintBox1.Width := PaintBox1.Width + 1;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.CheckBox1Click(Sender: TObject);
begin
  m.AutoSectionSize := CheckBox1.Checked;
end;

procedure TAdvSmoothMegaMenuEditorForm.CheckBox2Click(Sender: TObject);
begin
  m.TearOff := CheckBox2.Checked;
end;

procedure TAdvSmoothMegaMenuEditorForm.CheckBox3Click(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).toplayeritem) then
    TMenuNode(SelectedNode.Data).toplayeritem.Visible := CheckBox3.Checked;
end;

procedure TAdvSmoothMegaMenuEditorForm.CheckBox4Click(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
    TMenuNode(SelectedNode.Data).item.Enabled := CheckBox4.Checked;
end;

procedure TAdvSmoothMegaMenuEditorForm.CheckBox5Click(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
    TMenuNode(SelectedNode.Data).item.Checked := CheckBox5.Checked;
end;

procedure TAdvSmoothMegaMenuEditorForm.CheckBox6Click(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
    TMenuNode(SelectedNode.Data).item.EnableFill := CheckBox6.Checked;
end;

procedure TAdvSmoothMegaMenuEditorForm.CheckBox7Click(Sender: TObject);
begin
  m.ItemAppearance.AllowSelection := CheckBox7.Checked;
end;

procedure TAdvSmoothMegaMenuEditorForm.CheckBox8Click(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
    TMenuNode(SelectedNode.Data).item.Visible := CheckBox8.Checked;
end;

procedure TAdvSmoothMegaMenuEditorForm.CheckBox9Click(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).section) then
    TMenuNode(SelectedNode.Data).section.Visible := CheckBox9.Checked;
end;

procedure TAdvSmoothMegaMenuEditorForm.ClearTreeView;
var
  I: Integer;
  nd: TMenuNode;
begin
  for I := 0 to TreeView1.Items.Count - 1 do
  begin
    nd := TMenuNode(TreeView1.Items[I].Data);
    if Assigned(nd) then
      nd.Free;
  end;
  TreeView1.Items.Clear;
end;

procedure TAdvSmoothMegaMenuEditorForm.ComboBox10Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
    TMenuNode(SelectedNode.Data).item.TextLocation := TGDIPMenuLocation(comboBox10.ItemIndex);
end;

procedure TAdvSmoothMegaMenuEditorForm.ComboBox11Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
    TMenuNode(SelectedNode.Data).item.GraphicLeftName := ComboBox11.Text;
end;

procedure TAdvSmoothMegaMenuEditorForm.ComboBox12Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
    TMenuNode(SelectedNode.Data).item.GraphicRightName := ComboBox12.Text;
end;

procedure TAdvSmoothMegaMenuEditorForm.ComboBox13Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
    TMenuNode(SelectedNode.Data).item.ShortCut := TextToShortCut(ComboBox13.Text);
end;

procedure TAdvSmoothMegaMenuEditorForm.ComboBox1Change(Sender: TObject);
begin
  m.DropDownLocation := TGDIPMenuDropDownLocation(ComboBox1.Itemindex);
end;

procedure TAdvSmoothMegaMenuEditorForm.ComboBox2Change(Sender: TObject);
begin
  m.SectionLayout := TGDIPMenuSectionLayout(ComboBox2.ItemIndex);
end;

procedure TAdvSmoothMegaMenuEditorForm.ComboBox3Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).section) then
    TMenuNode(SelectedNode.Data).section.CaptionLocation := TGDIPMenuLocation(ComboBox3.ItemIndex);
end;

procedure TAdvSmoothMegaMenuEditorForm.ComboBox4Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).toplayeritem) then
    TMenuNode(SelectedNode.Data).toplayeritem.Align := TAlign(ComboBox4.ItemIndex);
end;

procedure TAdvSmoothMegaMenuEditorForm.ComboBox5Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).toplayeritem) then
    TMenuNode(SelectedNode.Data).toplayeritem.HTMLTextLocation := TGDIPMenuLocation(ComboBox5.ItemIndex);
end;

procedure TAdvSmoothMegaMenuEditorForm.ComboBox6Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
    TMenuNode(SelectedNode.Data).item.ItemType := TGDIPMenuSectionItemType(ComboBox6.ItemIndex);
end;

procedure TAdvSmoothMegaMenuEditorForm.ComboBox7Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
    TMenuNode(SelectedNode.Data).item.ControlType := TGDIPMenuSectionControlType(ComboBox7.ItemIndex);
end;

procedure TAdvSmoothMegaMenuEditorForm.ComboBox8Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
  begin
    TMenuNode(SelectedNode.Data).item.Control := TWinControl(ComboBox8.Items.Objects[ComboBox8.ItemIndex]);
    PaintBox1.Invalidate;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.ComboBox9Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
    TMenuNode(SelectedNode.Data).item.DetailTextLocation := TGDIPMenuLocation(ComboBox9.ItemIndex);
end;

procedure TAdvSmoothMegaMenuEditorForm.CorrectTop(lbl: TStaticText);
begin
  lbl.Top := Round(lbl.Tag - (lbl.Font.Size - 8) / 2);
end;

procedure TAdvSmoothMegaMenuEditorForm.DisableButtons;
begin
  SpeedButton1.Enabled := false;
  SpeedButton2.Enabled := false;
  SpeedButton3.Enabled := false;
  SpeedButton4.Enabled := false;
  SpeedButton5.Enabled := false;
  SpeedButton6.Enabled := false;
  SpeedButton9.Enabled := false;
  SpeedButton11.Enabled := false;
  SpeedButton10.Enabled := false;
end;

procedure TAdvSmoothMegaMenuEditorForm.Edit1Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).section) then
  begin
    TMenuNode(SelectedNode.Data).section.Caption := Edit1.Text;
    SelectedNode.Text := 'Section ' + inttostr(TMenuNode(SelectedNode.Data).section.Index) +  ' ' + Edit1.Text;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.Edit2Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).item) then
    TMenuNode(SelectedNode.Data).item.Data := Edit2.Text;
end;

procedure TAdvSmoothMegaMenuEditorForm.FillMenuProperties(m: TGDIPMenu; HidePnl: Boolean);
begin
  if Assigned(m) then
  begin
    if Hidepnl then
      HidePanels;

    CheckBox1.Checked := m.AutoSectionSize;
    CheckBox7.Checked := m.ItemAppearance.AllowSelection;
    SpinEdit1.Value := m.TearOffSize;
    SpinEdit2.Value := m.ItemAppearance.SeparatorSize;
    SpinEdit3.Value := m.ItemAppearance.BreakSize;
    Label13.Font.Assign(m.ItemAppearance.Font);
    CorrectTop(label13);
    Label14.Font.Assign(m.ItemAppearance.FontSelected);
    CorrectTop(label14);
    Label15.Font.Assign(m.ItemAppearance.FontHover);
    CorrectTop(label15);
    Label16.Font.Assign(m.ItemAppearance.FontDisabled);
    CorrectTop(label16);
    Label70.Font.Assign(m.ItemAppearance.DetailFont);
    CorrectTop(label70);
    StaticText6.Font.Assign(m.ItemAppearance.ShortCutFont);
    CorrectTop(Statictext6);
    SpinEdit4.Value := m.SectionMargin.Left;
    SpinEdit5.Value := m.SectionMargin.Top;
    SpinEdit6.Value := m.SectionMargin.Right;
    SpinEdit7.Value := m.SectionMargin.Bottom;
    ComboBox1.ItemIndex := Integer(m.DropDownLocation);
    SpinEdit8.Value := m.DropDownLeft;
    SpinEdit9.Value := m.DropDownTop;
    ComboBox2.ItemIndex := Integer(m.SectionLayout);
    CheckBox2.Checked := m.TearOff;
    AdvSmoothSpinEdit11.Value := m.ItemAppearance.ShadowOffset;
    PaintBox13.Color := m.ItemAppearance.ShadowColor;
    PaintBox14.Color := m.ItemAppearance.URLColor;
    //set properties
    if Hidepnl then
      pnlRoot.Visible := true;


    PaintBox2.Invalidate;
    PaintBox3.Invalidate;
    PaintBox4.Invalidate;
    PaintBox5.Invalidate;
    PaintBox6.Invalidate;
    PaintBox7.Invalidate;
    PaintBox8.Invalidate;
    PaintBox9.Invalidate;
    PaintBox13.Invalidate;
    PaintBox14.Invalidate;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.FillSectionItemProperties(
  si: TGDIPMenuSectionItem; hidepnl: boolean);
begin
  if Assigned(si) then
  begin
    if hidepnl then
      HidePanels;
    AdvSmoothSpinEdit3.Value := si.GroupIndex;
    CheckBox4.Checked := si.Enabled;
    CheckBox5.Checked := si.Checked;
    CheckBox6.Checked := si.EnableFill;
    ComboBox6.ItemIndex := integer(si.ItemType);
    ComboBox7.ItemIndex := integer(Si.ControlType);
    AdvSmoothSpinEdit4.Value := si.ControlIndent;
    ComboBox9.ItemIndex := Integer(si.DetailTextLocation);
    AdvSmoothSpinEdit5.Value := si.DetailTextLeft;
    AdvSmoothSpinEdit6.Value := si.DetailTextTop;
    AdvSmoothSpinEdit7.Value := si.SelectedTopLayerItem;
    AdvSmoothSpinEdit8.Value := si.HoverTopLayerItem;
    CheckBox8.Checked := si.Visible;
    ComboBox11.Text := si.GraphicLeftName;
    ComboBox12.Text := si.GraphicRightName;
    AdvSmoothSpinEdit9.Value := si.TextLeft;
    AdvSmoothSpinEdit10.Value := si.TextTop;
    ComboBox10.ItemIndex := integer(si.TextLocation);
    ComboBox8.ItemIndex := ComboBox8.Items.IndexOfObject(si.Control);
    Edit2.Text := si.Data;
    AdvSmoothSpinEdit12.Value := si.Tag;
    StaticText2.Caption := 'Item : ' + inttostr(si.Index);
    ComboBox13.ItemIndex := ComboBox13.Items.IndexOf(ShortCutToText(si.ShortCut));
    AdvSmoothSpinEdit13.Value := si.ImageIndex;
    if hidepnl then
      pnlSectionItem.Visible := true;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.FillSectionProperties(
  s: TGDIPMenuSection; hidepnl: Boolean);
begin
  if Assigned(s) then
  begin
    if Hidepnl then
      HidePanels;
    Edit1.Text := s.Caption;
    Label27.Font.Assign(s.CaptionFont);
    CorrectTop(Label27);
    ComboBox3.ItemIndex := Integer(s.CaptionLocation);
    checkbox9.Checked := s.Visible;
    SpinEdit10.Value := s.CaptionTop;
    SpinEdit11.Value := s.CaptionLeft;
    SpinEdit12.Value := s.CaptionSize;
    SpinEdit13.Value := s.Height;
    SpinEdit14.Value := s.Width;
    SpinEdit15.Value := s.ItemMargin.Left;
    SpinEdit16.Value := s.ItemMargin.Top;
    SpinEdit17.Value := s.ItemMargin.Right;
    SpinEdit18.Value := s.ItemMargin.Bottom;
    SpinEdit19.Value := s.ItemSpacing;
    StaticText3.Caption := 'Section : ' + inttostr(s.Index);
    if Hidepnl then
      pnlSection.Visible := true;

    PaintBox10.Invalidate;
    PaintBox11.Invalidate;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.FillTopLayerItemProperties(
  tl: TGDIPMenuTopLayerItem; hidepnl: Boolean);
begin
  if Assigned(tl) then
  begin
    if Hidepnl then
      HidePanels;
    //set properties
    CheckBox3.Checked := tl.Visible;
    SpinEdit20.Value := tl.Left;
    SpinEdit21.Value := tl.Top;
    SpinEdit22.Value := tl.Height;
    SpinEdit23.Value := tl.Width;
    ComboBox4.ItemIndex := Integer(tl.Align);
    ComboBox5.ItemIndex := integer(tl.HTMLTextLocation);
    Label50.Font.Assign(tl.HTMLTextFont);
    CorrectTop(Label50);
    AdvSmoothSpinEdit1.Value := tl.HTMLTextLeft;
    AdvSmoothSpinEdit2.Value := tl.HTMLTextTop;
    StaticText4.Caption := 'Top Layer Item : ' + inttostr(tl.Index);
    if Hidepnl then
      pnlTop.Visible := true;

    PaintBox12.Invalidate;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if (Mode = mNotSaved) and FMenuIsUpdated then
  begin
    case MessageDlg('Save Changes ?',mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
      begin
        SaveChanges;
        m.Free;
        ClearTreeView;
      end;
      mrNo:
      begin
       m.Free;
       ClearTreeView;
      end;
      mrCancel: Action := caNone;
    end;
  end
  else
  begin
    m.Free;
    ClearTreeView;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.FormCreate(Sender: TObject);
begin
//  DoubleBuffered := true;
//  pnlRoot.DoubleBuffered := true;
//  pnlSection.DoubleBuffered := true;
//  pnlSectionItem.DoubleBuffered := true;
//  pnlTop.DoubleBuffered := true;
//  pnlEmpty.DoubleBuffered := true;
  TreeView1.DoubleBuffered := true;
  ScrollBox1.DoubleBuffered := true;
  PaintBox1 := TCustomPaintBox.Create(ScrollBox1);
  PaintBox1.Parent := ScrollBox1;
  PaintBox1.Left := 1;
  PaintBox1.Top := 1;
  PaintBox1.OnMouseMove := PaintBox1MouseMove;
  Paintbox1.OnMouseDown := Paintbox1MouseDown;
  PaintBox1.OnMouseUp := PaintBox1MouseUp;
  PaintBox1.OnPaint := PaintBox1Paint;

  pnlEmpty.Caption := 'Please select an item in the treeview';
end;

procedure TAdvSmoothMegaMenuEditorForm.FormShow(Sender: TObject);
begin
  InitTreeview(true, true);
  UpdateMenu;
end;

function TAdvSmoothMegaMenuEditorForm.GetMenuNodeType(
  m: TMenuNode): TMenuNodeType;
begin
  result := ntRoot;
  if Assigned(m) then
  begin
    if Assigned(m.root) then
      result := ntRoot
    else if Assigned(m.section) then
      result := ntSection
    else if Assigned(m.item) then
      result := ntItem
    else if Assigned(m.toplayeritem) then
      result := ntTopLayerItem;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.HidePanels;
begin
  pnlRoot.Visible := false;
  pnlSection.Visible := false;
  pnlSectionItem.Visible := false;
  pnlTop.Visible := false;
end;

procedure TAdvSmoothMegaMenuEditorForm.Init(Create: Boolean);
var
  i: integer;
  f: TCustomForm;
  win: TWinControl;
  str: String;
  s: integer;
begin
  SpeedButton7.Visible := MenuPreview.HasMultipleMenus;
  SpeedButton8.Visible := MenuPreview.HasMultipleMenus;
  Mode := mNotSaved;
  if Create then
  begin
    m := TGDIPMenu.Create(MenuPreview.GetWinOwner);
    m.OnChange := MenuChanged;
    m.OnItemCheckChanged := MenuItemCheckChanged;
    m.BeginUpdate;
    m.Assign(MenuPreview);
    m.EndUpdate;
    m.ImageList := MenuPreview.ImageList;
    m.PictureContainer := MenuPreview.PictureContainer;
  end;

  FMenuIsUpdated := false;
  FAllowUpdate := false;

  ComboBox11.Items.Clear;
  ComboBox12.Items.Clear;
  ComboBox11.Items.Add('None');
  if Assigned(m.PictureContainer) then
    for I := 0 to m.PictureContainer.Items.Count - 1 do
      ComboBox11.Items.Add(m.PictureContainer.Items.Items[I].Name);

  ComboBox12.Items.Assign(ComboBox11.Items);

  ComboBox8.Items.Clear;
  ComboBox8.Items.Add('No Control');
  //controls
  win := m.GetWinOwner;
  if Assigned(m) and Assigned(win) then
  begin
   f := GetParentForm(m.GetWinOwner);
   if Assigned(f) then
   begin
     for I := 0 to f.ControlCount - 1 do
     begin
       ComboBox8.Items.AddObject(f.Controls[I].Name, f.Controls[I]);
     end;
   end;
  end;


  ComboBox13.Items.Clear;
  ComboBox13.Items.Add(ShortCutToText(0));
  s := TextToShortCut('CTRL+A');
  for I := s to s + 25 do
  begin
    str := ShortCutToText(I);
    if str <> '' then
      ComboBox13.Items.Add(str);
  end;

  s := TextToShortCut('F1');
  for I := s to s + 11 do
  begin
    str := ShortCutToText(I);
    if str <> '' then
      ComboBox13.Items.Add(str);
  end;

  s := TextToShortCut('CTRL+F1');
  for I := s to s + 11 do
  begin
    str := ShortCutToText(I);
    if str <> '' then
      ComboBox13.Items.Add(str);
  end;

  s := TextToShortCut('SHIFT+F1');
  for I := s to s + 11 do
  begin
    str := ShortCutToText(I);
    if str <> '' then
      ComboBox13.Items.Add(str);
  end;

  s := TextToShortCut('SHIFT+CTRL+F1');
  for I := s to s + 11 do
  begin
    str := ShortCutToText(I);
    if str <> '' then
      ComboBox13.Items.Add(str);
  end;

  s := TextToShortCut('CTRL+ALT+A');
  for I := s to s + 25 do
  begin
    str := ShortCutToText(I);
    if str <> '' then
      ComboBox13.Items.Add(str);
  end;

  str := ShortCutToText(45);
  if str <> '' then
    ComboBox13.Items.Add(str);

  str := ShortCutToText(8237);
  if str <> '' then
    ComboBox13.Items.Add(str);

  str := ShortCutToText(16429);
  if str <> '' then
    ComboBox13.Items.Add(str);

  str := ShortCutToText(46);
  if str <> '' then
    ComboBox13.Items.Add(str);

  str := ShortCutToText(8238);
  if str <> '' then
    ComboBox13.Items.Add(str);

  str := ShortCutToText(16430);
  if str <> '' then
    ComboBox13.Items.Add(str);

  str := ShortCutToText(32776);
  if str <> '' then
    ComboBox13.Items.Add(str);

  str := ShortCutToText(40968);
  if str <> '' then
    ComboBox13.Items.Add(str);

  FAllowUpdate := true;
end;

procedure TAdvSmoothMegaMenuEditorForm.InitTreeview(Expand, SelectRoot: Boolean; RootIndex: integer = - 1;
  RootTopIndex: integer = -1;RootItemIndex: integer = -1; RootSubItemIndex: integer = -1);
var
  sel, root, sec, secnode, topl: TTreeNode;
  nd: TMenuNode;
  I: Integer;
  C: Integer;
  str: String;
  stritem, strcontrol: String;
begin
  ClearTreeView;

  sel := nil;
  Root := TreeView1.Items.AddFirst(nil, 'Menu');
  nd := TMenuNode.Create;
  nd.root := m;
  Root.Data := nd;
  Root.StateIndex := 4;
  Self.Caption := 'TMS TAdvSmoothMegaMenu Editor : ['+m.RootCaption+']';

  for I := 0 to m.Sections.Count - 1 do
  begin
    nd := TMenuNode.Create;
    sec := TreeView1.Items.AddChild(Root, 'Section ' + inttostr(I) + ' ' + m.Sections[I].Caption);
    nd.section := m.Sections[I];
    sec.Data := nd;
    sec.StateIndex := 3;
    if RootItemIndex = sec.Index then
      sel := sec;

    for C := 0 to m.Sections[I].Items.Count - 1 do
    begin
      nd := TMenuNode.Create;
      nd.item := m.Sections[I].Items[C];
      str := nd.item.Text;
      str := m.GetStrippedHTMLText(nd.item.Text);
      str := StringReplace(str, #13, ' ', [rfReplaceAll]);
      str := StringReplace(str, #10, ' ', [rfReplaceAll]);
      if Length(str) > 10 then
        str := Copy(str, 0, 10) + ' ...';

      case nd.item.ItemType of
        itNormal: stritem := 'Normal';
        itHeader: stritem := 'Header';
        itSeparator: stritem := 'Separator';
        itBreak: stritem := 'Break';
        itLineSeparator: stritem := 'Line Separator';
        itLineBreak: stritem := 'Line Break';
      end;

      case nd.item.ControlType of
        ctNone: strcontrol := 'None';
        ctControl: strControl := 'Control';
        ctCheckBox: strControl := 'CheckBox';
        ctRadioButton: strControl := 'RadioButton';
        ctEdit: strControl := 'Edit';
      end;

      secnode := TreeView1.Items.AddChild(Sec, 'Item ' + inttostr(C) + ' ['+str+']');
      TreeView1.Items.AddChild(Secnode, 'Item Type : ' + stritem);
      TreeView1.Items.AddChild(Secnode, 'Control Type : ' + strcontrol);
      secnode.Data := nd;
      secnode.StateIndex := 2;
      if (RootItemIndex = sec.Index) and (RootSubItemIndex = secnode.Index) then
        sel := secnode;
    end;
  end;

  for I := 0 to m.TopLayerItems.Count - 1 do
  begin
    nd := TMenuNode.Create;
    topl := TreeView1.Items.AddChild(Root, 'Toplayer Item ' + inttostr(I));
    nd.toplayeritem := m.TopLayerItems[I];
    topl.Data := nd;
    topl.StateIndex := 1;
    if RootTopIndex = topl.Index then
      sel := topl;
  end;

  RootNode := Root;
  if RootIndex = RootNode.Index then
    sel := RootNode;

  if Expand then
    TreeView1.FullExpand;

  if SelectRoot then
    Root.Selected := true;

  if Assigned(sel) then
    Sel.Selected := true;
end;

procedure TAdvSmoothMegaMenuEditorForm.MenuChanged(Sender: TOBject);
begin
  UpdateMenu;
  if not FMenuIsUpdated then
    FMenuIsUpdated := FAllowUpdate;
end;

procedure TAdvSmoothMegaMenuEditorForm.MenuItemCheckChanged(Sender: TObject;
  item: TGDIPMenuSectionItem);
begin
  if Assigned(SelectedNode) and Assigned(SelectedNode.Data) then
    if Assigned(TMenuNode(SelectedNode.Data)) then
      if TMenuNode(SelectedNode.Data).item = item then
        FillSectionItemProperties(item, false);
end;

procedure TAdvSmoothMegaMenuEditorForm.PaintBox10Paint(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).section) then
    PaintFill(PaintBox10, TMenuNode(SelectedNode.Data).section.CaptionFill);
end;

procedure TAdvSmoothMegaMenuEditorForm.PaintBox11Paint(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).section) then
    PaintFill(PaintBox11, TMenuNode(SelectedNode.Data).section.BackGroundFill);
end;

procedure TAdvSmoothMegaMenuEditorForm.PaintBox12Paint(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).toplayeritem) then
    PaintFill(PaintBox12, TMenuNode(SelectedNode.Data).toplayeritem.Fill);
end;

procedure TAdvSmoothMegaMenuEditorForm.PaintBox13Paint(Sender: TObject);
begin
  PaintBox13.Canvas.Brush.Color := PaintBox13.Color;
  PaintBox13.Canvas.FillRect(paintbox13.ClientRect);
end;

procedure TAdvSmoothMegaMenuEditorForm.PaintBox14Paint(Sender: TObject);
begin
  PaintBox14.Canvas.Brush.Color := PaintBox14.Color;
  PaintBox14.Canvas.FillRect(paintbox14.ClientRect);
end;

procedure TAdvSmoothMegaMenuEditorForm.PaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  tl := m.XYToTopLayerItem(X, Y);
  if tl <> nil then
  begin
    SelectTopLayerTreeView(tl);
    DoTLdrag := true;
    TLLeft := Abs(tl.Left - X);
    TLTop := Abs(tl.Top - Y);
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.PaintBox1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  tlh: TGDIPMenuTopLayerItem;
begin
  tlh := m.XYToTopLayerItem(X, Y);
  if tlh <> nil then
  begin
    Screen.Cursor := crSizeAll;
    if DoTLDrag and Assigned(tl) then
    begin
      m.BeginUpdate;
      SpinEdit20.Value := X - TLLEft;
      SpinEdit21.Value := Y - TLtop;
      m.EndUpdate;
    end
  end
  else
  begin
    Screen.Cursor := crDefault;
    FAllowUpdate := false;
    m.DoMouseMove(Shift, X, Y);
    FAllowUpdate := true;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.PaintBox1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if DoTLDrag then
    DoTLdrag := false
  else
  begin
    FAllowUpdate := false;
    m.DoMouseUp(Button, Shift, X, Y);
    if Assigned(m.SelectedItem.item) then
      SelectItemTreeView(m.SelectedItem.item);
    FAllowUpdate := true;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.PaintBox1Paint(Sender: TObject);
var
  g: TGPGraphics;
  I: Integer;
  c: TControl;
  K: Integer;
  bmp: TGPBitmap;
  gbmp: TGPGraphics;
  hdc: HWND;
begin
  g := TGPGraphics.Create(PaintBox1.Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
  m.Draw(g, PaintBox1.ClientRect);
  for I := 0 to m.Sections.Count - 1 do
  begin
    for K := 0 to m.Sections[I].Items.Count - 1 do
    begin
      if (m.Sections[I].Items[K].ControlType = ctControl) or (m.Sections[I].Items[K].ControlType = ctEdit) then
      begin
        c := m.Sections[I].Items[K].Control;
        if Assigned(c) then
        begin
          bmp := TGPBitmap.Create(c.Width, c.Height);
          gbmp := TGPGraphics.Create(bmp);
          hdc := gbmp.GetHDC;
          if c is TWinControl then
            (c as TWinControl).PaintTo(hdc, 0, 0);
          gbmp.ReleaseHDC(hdc);
          gbmp.Free;
          g.DrawImage(bmp, c.Left, c.Top);
          bmp.Free;
        end;
      end;
    end;
  end;
  g.Free;
end;

procedure TAdvSmoothMegaMenuEditorForm.PaintBox2Paint(Sender: TObject);
begin
  PaintFill(PaintBox2, m.ItemAppearance.SeparatorFill);
end;

procedure TAdvSmoothMegaMenuEditorForm.PaintBox3Paint(Sender: TObject);
begin
  PaintFill(PaintBox3, m.ItemAppearance.BreakFill);
end;

procedure TAdvSmoothMegaMenuEditorForm.PaintBox4Paint(Sender: TObject);
begin
  PaintFill(PaintBox4, m.ItemAppearance.FillSelected);
end;

procedure TAdvSmoothMegaMenuEditorForm.PaintBox5Paint(Sender: TObject);
begin
  PaintFill(PaintBox5, m.ItemAppearance.Fill);
end;

procedure TAdvSmoothMegaMenuEditorForm.PaintBox6Paint(Sender: TObject);
begin
  PaintFill(PaintBox6, m.ItemAppearance.FillDisabled);
end;

procedure TAdvSmoothMegaMenuEditorForm.PaintBox7Paint(Sender: TObject);
begin
  PaintFill(PaintBox7, m.ItemAppearance.FillHover);
end;

procedure TAdvSmoothMegaMenuEditorForm.PaintBox8Paint(Sender: TObject);
begin
  PaintFill(PaintBox8, m.TearOffFill);
end;

procedure TAdvSmoothMegaMenuEditorForm.PaintBox9Paint(Sender: TObject);
begin
  Paintfill(PaintBox9, m.ContentFill);
end;

procedure TAdvSmoothMegaMenuEditorForm.PaintFill(pb: TPaintBox; f: TGDIPFill);
var
  g: TGPGraphics;
begin
  g := TGPGraphics.Create(pb.Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  f.Fill(g, MakeRect(0, 0, pb.Width - 1, pb.Height - 1));
  g.Free;
end;

procedure TAdvSmoothMegaMenuEditorForm.SaveChanges;
begin
  FMenuPreview.Assign(m);
end;

procedure TAdvSmoothMegaMenuEditorForm.SelectItemTreeView(
  si: TGDIPMenuSectionItem);
var
  I: Integer;
begin
  for I := 0 to TreeView1.Items.Count - 1 do
  begin
    if Assigned(TreeView1.Items[I].Data) then
    begin
      if Assigned(TMenuNode(TreeView1.Items[I].Data).Item) then
      begin
        if TMenuNode(TreeView1.Items[I].Data).item = si then
        begin
          TreeView1.SetFocus;
          TreeView1.Items[I].Selected := true;
          break;
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.SelectTopLayerTreeView(
  tl: TGDIPMenuTopLayerItem);
var
  I: Integer;
begin
  for I := 0 to TreeView1.Items.Count - 1 do
  begin
    if Assigned(TreeView1.Items[I].Data) then
    begin
      if Assigned(TMenuNode(TreeView1.Items[I].Data).toplayeritem) then
      begin
        if TMenuNode(TreeView1.Items[I].Data).toplayeritem = tl then
        begin
          TreeView1.SetFocus;
          TreeView1.Items[I].Selected := true;
          break;
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.SetRootNode(const Value: TTreeNode);
begin
  FRootNode := Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SetSelectedNode(const Value: TTreeNode);
begin
  disableButtons;
  FSelectedNode := Value;
  if Assigned(FSelectedNode) and Assigned(FSelectedNode.Data) then
  begin
    SpeedButton2.Enabled := true;
    SpeedButton6.Enabled := true;
    if Assigned(TMenuNode(FSelectedNode.Data).section) then
    begin
      SpeedButton1.Enabled := true;
      SpeedButton4.Enabled := true;
      SpeedButton9.Enabled := true;
      SpeedButton11.Enabled := true;
      SpeedButton10.Enabled := true;
    end
    else if Assigned(TMenuNode(FSelectedNode.Data).item) then
    begin
      SpeedButton3.Enabled := true;
      SpeedButton9.Enabled := true;
      SpeedButton11.Enabled := true;
      SpeedButton10.Enabled := true;
    end
    else if Assigned(TMenuNode(FSelectedNode.Data).toplayeritem) then
      SpeedButton5.Enabled := true;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpeedButton10Click(Sender: TObject);
begin
  if Assigned(SelectedNode) and Assigned(SelectedNode.Data) then
  begin
    if Assigned(TMenuNode(SelectedNode.Data).item) then
    begin
      TMenuNode(SelectedNode.Data).item.Index := Min(TMenuNode(SelectedNode.Data).item.Index + 1, TMenuNode(SelectedNode.Data).item.Section.Items.Count - 1);
      InitTreeview(true, false, -1, -1, SelectedNode.Parent.Index, Min(SelectedNode.Parent.Count - 1, SelectedNode.Index + 1));
      UpdateMenu;
      FMenuIsUpdated := True;
    end
    else if Assigned(TMenuNode(SelectedNode.Data).section) then
    begin
      TMenuNode(SelectedNode.Data).section.Index := Min(TMenuNode(SelectedNode.Data).section.Index + 1, TMenuNode(RootNode.Data).root.Sections.Count - 1);
      InitTreeview(true, false, -1, SelectedNode.Parent.Index, Min(SelectedNode.Parent.Count - 1, SelectedNode.Index + 1), -1);
      UpdateMenu;
      FMenuIsUpdated := True;
    end;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpeedButton11Click(Sender: TObject);
begin
  if Assigned(SelectedNode) and Assigned(SelectedNode.Data) then
  begin
    if Assigned(TMenuNode(SelectedNode.Data).item) then
    begin
      TMenuNode(SelectedNode.Data).item.Section.Items.Insert(TMenuNode(SelectedNode.Data).item.Index).Assign(TMenuNode(SelectedNode.Data).item);
      InitTreeview(true, false, -1, -1, SelectedNode.Parent.Index, Min(SelectedNode.Parent.Count - 1, SelectedNode.Index + 1));
      UpdateMenu;
      FMenuIsUpdated := True;
    end
    else if Assigned(TMenuNode(SelectedNode.Data).section) then
    begin
      TMenuNode(RootNode.Data).root.sections.Insert(TMenuNode(SelectedNode.Data).section.Index).Assign(TMenuNode(SelectedNode.Data).section);
      InitTreeview(true, false, -1, -1, Min(SelectedNode.Parent.Count - 1, SelectedNode.Index + 1), -1);
      UpdateMenu;
      FMenuIsUpdated := True;
    end;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpeedButton1Click(Sender: TObject);
var
  cnt, i: integer;
begin
  for I := 0 to SelectedNode.Count - 1 do
  begin
    TMenuNode(SelectedNode.Item[I].Data).Free;
  end;

  TMenuNode(SelectedNode.Data).section.Free;
  TMenuNode(SelectedNode.Data).Free;
  i := SelectedNode.Index;
  cnt := SelectedNode.Parent.Count;
  SelectedNode.Free;
  if i < cnt - 1 then
    InitTreeview(true, false, -1, -1, i, -1)
  else
    InitTreeView(true, false, -1, -1, i - 1, - 1);

  UpdateMenu;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpeedButton2Click(Sender: TObject);
var
  ndnew: TMenuNode;
  sec: TTreeNode;
begin
  m.Sections.Add;
  sec := TreeView1.Items.AddChild(RootNode, 'Section ' + inttostr(m.Sections.Count - 1) + ' ' + m.Sections[m.Sections.Count - 1].Caption);
  ndnew := TMenuNode.Create;
  ndnew.section := m.Sections[m.Sections.Count - 1];
  sec.Data := ndnew;
  InitTreeView(true, false, -1, -1, sec.Index, -1);
  UpdateMenu;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpeedButton3Click(Sender: TObject);
var
  cnt, i, p: integer;
begin
  TMenuNode(SelectedNode.Data).item.Free;
  TMenuNode(SelectedNode.Data).Free;
  i := SelectedNode.Index;
  p := SelectedNode.Parent.Index;
  cnt := SelectedNode.Parent.Count;
  SelectedNode.Free;
  if i < cnt - 1 then
    InitTreeview(true, false, -1, -1, p, i)
  else
    InitTreeView(true, false, -1, -1, p, i - 1);

  UpdateMenu;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpeedButton4Click(Sender: TObject);
var
  nd: TMenuNode;
  ndnew: TMenuNode;
  sec: TTreeNode;
begin
  nd := TMenuNode(SelectedNode.Data);
  with nd.section.Items.Add do
  begin
    Text := 'Item ' + inttostr(nd.section.Items.Count - 1);
  end;
  sec := TreeView1.Items.AddChild(SelectedNode, 'Section Item ' + inttostr(nd.section.Items.Count - 1));
  ndnew := TMenuNode.Create;
  ndnew.item := nd.section.Items[nd.section.Items.Count - 1];
  sec.Data := ndnew;
  InitTreeView(true, false, -1, -1, SelectedNode.Index, -1);
  UpdateMenu;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpeedButton5Click(Sender: TObject);
var
  cnt, i: integer;
begin
  TMenuNode(SelectedNode.Data).toplayeritem.Free;
  TMenuNode(SelectedNode.Data).Free;
  i := SelectedNode.Index;
  cnt := SelectedNode.Parent.Count;
  SelectedNode.Parent.Index;
  SelectedNode.Free;
  if i < cnt - 1 then
    InitTreeview(true, false, -1, i, -1, -1)
  else
    InitTreeView(true, false, -1, i - 1, - 1);

  UpdateMenu;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpeedButton6Click(Sender: TObject);
var
  ndnew: TMenuNode;
  sec: TTreeNode;
begin
  m.TopLayerItems.Add;
  sec := TreeView1.Items.AddChild(RootNode, 'Top Layer Item ' + inttostr(m.TopLayerItems.Count - 1));
  ndnew := TMenuNode.Create;
  ndnew.toplayeritem := m.TopLayerItems[m.TopLayerItems.Count - 1];
  sec.Data := ndnew;
  InitTreeView(true, false, -1, sec.Index, -1, -1);
  UpdateMenu;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpeedButton7Click(Sender: TObject);
var
  mp: TGDIPMenu;
begin
  if Assigned(MenuPreview) then
    mp := MenuPreview.GetPreviousMenu
  else
    Exit;

  if Assigned(mp) then
  begin
    if Assigned(m) and FMenuIsUpdated then
    begin
      case MessageDlg('Save Changes ?',mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
        mrYes:
        begin
          SaveChanges;
          m.Free;
          ClearTreeView;
        end;
        mrNo:
        begin
          m.Free;
          ClearTreeView;
        end;
        mrCancel: Exit;
      end;
    end;

    DoRefresh := true;
    MenuPreview := mp;
    Init(true);
    InitTreeview(true, true);
    //update menu
    UpdateMenu;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpeedButton8Click(Sender: TObject);
var
  mp: TGDIPMenu;
begin
  if Assigned(MenuPreview) then
    mp := MenuPreview.GetNextMenu
  else
    Exit;

  if Assigned(mp) then
  begin
    if Assigned(m) and FMenuIsUpdated then
    begin
      case MessageDlg('Save Changes ?',mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
        mrYes:
        begin
          SaveChanges;
          m.Free;
          ClearTreeView;
        end;
        mrNo:
        begin
          m.Free;
          ClearTreeView;
        end;
        mrCancel: Exit;
      end;
    end;

    DoRefresh := true;
    MenuPreview := mp;
    Init(true);
    InitTreeview(true, true);
    //update menu
    UpdateMenu;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpeedButton9Click(Sender: TObject);
begin
  if Assigned(SelectedNode) and Assigned(SelectedNode.Data) then
  begin
    if Assigned(TMenuNode(SelectedNode.Data).item) then
    begin
      TMenuNode(SelectedNode.Data).item.Index := Max(TMenuNode(SelectedNode.Data).item.Index - 1, 0);
      InitTreeview(true, false, -1, -1, SelectedNode.Parent.Index, Max(0, SelectedNode.Index - 1));
      UpdateMenu;
      FMenuIsUpdated := True;
    end
    else if Assigned(TMenuNode(SelectedNode.Data).section) then
    begin
      TMenuNode(SelectedNode.Data).section.Index := Max(TMenuNode(SelectedNode.Data).section.Index - 1, 0);
      InitTreeview(true, false, -1, SelectedNode.Parent.Index, Max(0, SelectedNode.Index - 1), -1);
      UpdateMenu;
      FMenuIsUpdated := True;
    end;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit10Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).section) then
    TMenuNode(SelectedNode.Data).section.CaptionTop := SpinEdit10.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit11Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).section) then
    TMenuNode(SelectedNode.Data).section.CaptionLeft := SpinEdit11.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit12Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).section) then
    TMenuNode(SelectedNode.Data).section.CaptionSize := SpinEdit12.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit13Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).section) then
    TMenuNode(SelectedNode.Data).section.Height := SpinEdit13.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit14Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).section) then
    TMenuNode(SelectedNode.Data).section.Width := SpinEdit14.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit15Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).section) then
    TMenuNode(SelectedNode.Data).section.ItemMargin.Left := SpinEdit15.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit16Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).section) then
    TMenuNode(SelectedNode.Data).section.ItemMargin.Top := SpinEdit16.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit17Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).section) then
    TMenuNode(SelectedNode.Data).section.ItemMargin.right := SpinEdit17.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit18Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).section) then
    TMenuNode(SelectedNode.Data).section.ItemMargin.Bottom := SpinEdit18.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit19Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).section) then
    TMenuNode(SelectedNode.Data).section.ItemSpacing := SpinEdit19.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit1Change(Sender: TObject);
begin
  m.TearOffSize := SpinEdit1.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit20Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).toplayeritem) then
    TMenuNode(SelectedNode.Data).toplayeritem.Left := SpinEdit20.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit21Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).toplayeritem) then
    TMenuNode(SelectedNode.Data).toplayeritem.Top := SpinEdit21.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit22Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).toplayeritem) then
    TMenuNode(SelectedNode.Data).toplayeritem.Height := SpinEdit22.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit23Change(Sender: TObject);
begin
  if Assigned(TMenuNode(SelectedNode.Data).toplayeritem) then
    TMenuNode(SelectedNode.Data).toplayeritem.Width := SpinEdit23.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit2Change(Sender: TObject);
begin
  m.ItemAppearance.SeparatorSize := SpinEdit2.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit3Change(Sender: TObject);
begin
  m.ItemAppearance.BreakSize := SpinEdit3.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit4Change(Sender: TObject);
begin
  m.SectionMargin.Left := SpinEdit4.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit5Change(Sender: TObject);
begin
  m.SectionMargin.Top := SpinEdit5.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit6Change(Sender: TObject);
begin
  m.SectionMargin.Right := SpinEdit6.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit7Change(Sender: TObject);
begin
  m.SectionMargin.Bottom := SpinEdit7.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit8Change(Sender: TObject);
begin
  m.DropDownLeft := spinEdit8.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.SpinEdit9Change(Sender: TObject);
begin
  m.DropDownTop := spinEdit9.Value;
end;

procedure TAdvSmoothMegaMenuEditorForm.StartFillEditor(f: TGDIPFill);
var
  frm: TAdvSmoothFillEditorDialog;
begin
  frm := TAdvSmoothFillEditorDialog.Create(Self);
  frm.Fill := f;
  frm.Execute;
  frm.Free;
  UpdateMenu;
end;

procedure TAdvSmoothMegaMenuEditorForm.StartHTMLEditor(var str: string);
var
  htmledit: TAdvSmoothHTMLEditorForm;
begin
  htmledit := TAdvSmoothHTMLEditorForm.Create(Self);
  try
    htmledit.memo1.lines.Text := str;
    htmledit.ShowModal;
    str := htmledit.memo1.lines.Text;
  finally
    htmledit.Free;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.TreeView1Change(Sender: TObject;
  Node: TTreeNode);
var
  nd: TMenuNode;
  it: TCurrentItem;
  hidepnl: Boolean;
begin
  hidepnl := true;

  if Assigned(Node.Data) and not DoRefresh then
  begin
    if Assigned(SelectedNode) then
      hidepnl := GetMenuNodeType(TMenuNode(SelectedNode.Data)) <> GetMenuNodeType(TMenuNode(Node.Data));
  end;

  if DoRefresh then
    DoRefresh := false;

  SelectedNode := Node;
  if Assigned(SelectedNode) then
    SelectedNode.Selected := true;

  if Assigned(SelectedNode.Data) then
  begin
    nd := TMenuNode(SelectedNode.Data);
    if Assigned(nd) then
    begin
      if Assigned(nd.root) then
        FillMenuProperties(nd.root, hidepnl)
      else if Assigned(nd.section) then
        FillSectionProperties(nd.section, hidepnl)
      else if Assigned(nd.item) then
      begin
        FillSectionItemProperties(nd.item, hidepnl);
        it.item := nd.item;
        it.section := nd.item.Section;
        m.SelectedItem := it;
        UpdateMenu;
      end
      else if Assigned(nd.toplayeritem) then
        FillTopLayerItemProperties(nd.toplayeritem, hidepnl)
    end
  end
  else
  begin
    HidePanels;
    pnlEmpty.Visible := true;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.TreeView1DragDrop(Sender,
  Source: TObject; X, Y: Integer);
var
  AnItem: TTreeNode;
  nd, ndsel: TMenuNode;
begin
  AnItem := TreeView1.GetNodeAt(X, Y);
  ndsel := nil;
  if Assigned(TreeView1.Selected) then
    if Assigned(TreeView1.Selected.Data) then
      ndsel := TMenuNode(TreeView1.Selected.Data);

  nd := nil;
  if Assigned(AnItem) then
    if Assigned(AnItem.Data) then
      nd := TMenuNode(AnItem.Data);

  if Assigned(nd) and Assigned(ndsel) then
  begin
    if (Assigned(nd.item) or Assigned(nd.section)) and Assigned(ndsel.item) then
    begin
      if Assigned(nd.section) then
      begin
        nd.section.Items.Add.Assign(ndsel.item);
        ndsel.item.Free;
        InitTreeView(true, true, -1, -1, nd.Section.Index, ndsel.item.Index);
      end
      else
      begin
        ndsel.item.Index := nd.item.Index;
        InitTreeView(true, true, -1, -1, ndsel.item.Section.Index, ndsel.item.Index);
      end;
    end;
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.TreeView1DragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  AnItem: TTreeNode;
  nd, ndsel: TMenuNode;
begin
  AnItem := TreeView1.GetNodeAt(X, Y);
  ndsel := nil;
  if Assigned(TreeView1.Selected) then
    if Assigned(TreeView1.Selected.Data) then
      ndsel := TMenuNode(SelectedNode.Data);

  nd := nil;
  if Assigned(AnItem) then
    if Assigned(AnItem.Data) then
      nd := TMenuNode(AnItem.Data);

  Accept := false;
  if Assigned(nd) and Assigned(ndsel) then
  begin
    if (Assigned(nd.item) or Assigned(nd.section)) and Assigned(ndsel.item) then
    begin
      Accept := true;
    end
  end;
end;

procedure TAdvSmoothMegaMenuEditorForm.UpdateMenu;
begin
  CalculateSize;
  m.Init(PaintBox1.ClientRect, false, true);
  PaintBox1.Invalidate;
end;

{ TCustomPaintBox }

procedure TCustomPaintBox.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FAllowUpdate := false;
  DoTLdrag := false;
  m.DoCMMouseLeave(msg);
  FAllowUpdate := true;
end;

end.
