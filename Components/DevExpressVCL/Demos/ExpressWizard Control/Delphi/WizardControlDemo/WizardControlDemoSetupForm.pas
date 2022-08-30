unit WizardControlDemoSetupForm;

{$I cxVer.inc}

interface

uses
{$IFDEF EXPRESSSKINS}
  dxSkinsForm,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxContainer, cxEdit, Menus, StdCtrls, cxButtons, cxCheckBox, cxListBox,
  dxCustomWizardControl, dxWizardControl, cxGroupBox, cxRadioGroup, cxTextEdit,
  cxMaskEdit, cxDropDownEdit, cxLabel, ComCtrls, cxTreeView;

type
  TWizardControlDemoSetupForm = class(TForm)
    btnStartDemo: TcxButton;
    cbSkinForm: TcxCheckBox;
    lbChooseSkin: TcxLabel;
    rgTransitionEffect: TcxRadioGroup;
    rgViewStyle: TcxRadioGroup;
    tvSkins: TcxTreeView;
    procedure cbSkinFormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tvSkinsChange(Sender: TObject; Node: TTreeNode);
    procedure tvSkinsCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
  private
  {$IFDEF EXPRESSSKINS}
    FSkinController: TdxSkinController;
  {$ENDIF}
    function GetSkinForm: Boolean;
    function GetTransitionEffect: TdxWizardControlTransitionEffect;
    function GetViewStyle: TdxWizardControlViewStyle;
    procedure ChangeStyle(APainter: TcxCustomLookAndFeelPainter);
  public
    property SkinForm: Boolean read GetSkinForm;
    property TransitionEffect: TdxWizardControlTransitionEffect read GetTransitionEffect;
    property ViewStyle: TdxWizardControlViewStyle read GetViewStyle;
  end;

implementation

{$R *.dfm}

const
  sWizardControlDemoSetupFormNodeNameStandard = 'Standard';
  sWizardControlDemoSetupFormNodeNameSkins = 'Skins';

{ TWizardControlDemoSetupForm }

function TWizardControlDemoSetupForm.GetSkinForm: Boolean;
begin
  Result := cbSkinForm.Checked and cbSkinForm.Enabled;
end;
 
function TWizardControlDemoSetupForm.GetTransitionEffect:
  TdxWizardControlTransitionEffect;
const
  TransitionEffectMap: array [0..2] of TdxWizardControlTransitionEffect =
    (wcteNone, wcteFade, wcteSlide);
begin
  Result := TransitionEffectMap[rgTransitionEffect.ItemIndex];
end;

function TWizardControlDemoSetupForm.GetViewStyle: TdxWizardControlViewStyle;
const
  ViewStyleMap: array[0..1] of TdxWizardControlViewStyle = (wcvsAero, wcvsWizard97);
begin
  Result := ViewStyleMap[rgViewStyle.ItemIndex];
end;
 
procedure TWizardControlDemoSetupForm.ChangeStyle(APainter: TcxCustomLookAndFeelPainter);
begin
  cbSkinForm.Enabled := False;
  case APainter.LookAndFeelStyle of
    lfsNative:
      RootLookAndFeel.NativeStyle := True;
    lfsSkin:
      begin
        RootLookAndFeel.NativeStyle := False;
        RootLookAndFeel.SkinName := APainter.LookAndFeelName;
        cbSkinForm.Enabled := True;
      end;
  else
    begin
      RootLookAndFeel.NativeStyle := False;
      RootLookAndFeel.SkinName := '';
      RootLookAndFeel.SetStyle(APainter.LookAndFeelStyle);
    end;
  end;
end;

procedure TWizardControlDemoSetupForm.cbSkinFormClick(Sender: TObject);
begin
{$IFDEF EXPRESSSKINS}
  if cbSkinForm.Checked then
    FSkinController := TdxSkinController.Create(Self)
  else
    FreeAndNil(FSkinController);
{$ENDIF}
end;

procedure TWizardControlDemoSetupForm.FormCreate(Sender: TObject);
var
  I: Integer;
  AStandardNode, ASkinsNode: TTreeNode;
  ABufStandardNode, ABufSkinsNode: TTreeNode;
  APainter: TcxCustomLookAndFeelPainter;
begin
  ABufSkinsNode := TTreeNode.Create(tvSkins.Items);
  try
    ASkinsNode := tvSkins.Items.Add(ABufSkinsNode,
      sWizardControlDemoSetupFormNodeNameSkins);
    ABufStandardNode := TTreeNode.Create(tvSkins.Items);
    try
      AStandardNode := tvSkins.Items.Add(ABufStandardNode,
        sWizardControlDemoSetupFormNodeNameStandard);
      for I := 0 to cxLookAndFeelPaintersManager.Count - 1 do
      begin
        APainter := cxLookAndFeelPaintersManager.Items[I];
        if APainter.LookAndFeelStyle = lfsSkin then
          tvSkins.Items.AddChildObject(ASkinsNode, APainter.LookAndFeelName, APainter)
        else
          tvSkins.Items.AddChildObject(AStandardNode, APainter.LookAndFeelName, APainter);
      end;
      tvSkins.FullExpand;

      if ASkinsNode.Count = 0 then
      begin
        AStandardNode.getFirstChild.Selected := True;
        ASkinsNode.Delete;
      end
      else
        ASkinsNode.getFirstChild.Selected := True;
    finally
      ABufStandardNode.Free;
    end;
  finally
    ABufSkinsNode.Free;
  end;
end;

procedure TWizardControlDemoSetupForm.tvSkinsChange(Sender: TObject; Node: TTreeNode);
begin
  if tvSkins.Selected.Data <> nil then
    ChangeStyle(TcxCustomLookAndFeelPainter(tvSkins.Selected.Data));
end;

procedure TWizardControlDemoSetupForm.tvSkinsCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if Node.Data = nil then
    Sender.Canvas.Font.Style := [fsBold];
end;

end.


