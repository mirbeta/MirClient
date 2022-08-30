unit Options;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, Buttons, ExtCtrls, dxorgchr, cxGraphics, cxLookAndFeels,
  cxLookAndFeelPainters, Menus, cxControls;

type
  TOptionsForm = class(TForm)
    Bevel1: TBevel;
    cbButtons: TCheckBox;
    cbCanDrag: TCheckBox;
    cbCenter: TCheckBox;
    cbEdit: TCheckBox;
    cbFocus: TCheckBox;
    cbGrow: TCheckBox;
    cbInsDel: TCheckBox;
    cbLeft: TCheckBox;
    cbLower: TCheckBox;
    cbRight: TCheckBox;
    cbSelect: TCheckBox;
    cbShowDrag: TCheckBox;
    cbShowImages: TCheckBox;
    cbUpper: TCheckBox;
    cbVCenter: TCheckBox;
    cbWrap: TCheckBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    seLineWidth: TSpinEdit;
    seX: TSpinEdit;
    seY: TSpinEdit;
    BitBtn1: TButton;
    BitBtn2: TButton;
    procedure BitBtn2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  end;

var
  OptionsForm: TOptionsForm;

implementation

uses
  main;

{$R *.DFM}

type
  TdxCustomOrgChartAccess = class(TdxCustomOrgChart);

{ TOptionsForm }

procedure TOptionsForm.FormActivate(Sender: TObject);
begin
  with TdxCustomOrgChartAccess(MainForm.ActiveOrgChart) do
  begin
    cbLeft.Checked := emLeft in EditMode;
    cbCenter.Checked := emCenter in EditMode;
    cbRight.Checked := emRight in EditMode;
    cbVCenter.Checked := emVCenter in EditMode;
    cbWrap.Checked := emWrap in EditMode;
    cbUpper.Checked := emUpper in EditMode;
    cbLower.Checked := emLower in EditMode;
    cbGrow.Checked := emGrow in EditMode;

    cbSelect.Checked := ocSelect in Options;
    cbFocus.Checked := ocFocus in Options;
    cbButtons.Checked := ocButtons in Options;
    cbEdit.Checked := ocEdit in Options;
    cbCanDrag.Checked := ocCanDrag in Options;
    cbShowDrag.Checked := ocshowDrag in Options;
    cbInsDel.Checked := ocInsDel in Options;

    seX.Value := IndentX;
    seY.Value := IndentY;
    seLineWidth.Value := LineWidth;
    cbShowImages.Checked := not(Images = nil);
  end;
end;

procedure TOptionsForm.BitBtn2Click(Sender: TObject);
begin
  with TdxCustomOrgChartAccess(MainForm.ActiveOrgChart) do
  begin
    if cbLeft.Checked then EditMode := EditMode + [emLeft] else EditMode := EditMode - [emLeft];
    if cbCenter.Checked then EditMode := EditMode + [emCenter] else EditMode := EditMode - [emCenter];
    if cbRight.Checked then EditMode := EditMode + [emRight] else EditMode := EditMode - [emRight];
    if cbVCenter.Checked then EditMode := EditMode + [emVCenter] else EditMode := EditMode - [emVCenter];
    if cbWrap.Checked then EditMode := EditMode + [emWrap] else EditMode := EditMode - [emWrap];
    if cbUpper.Checked then EditMode := EditMode + [emUpper] else EditMode := EditMode - [emUpper];
    if cbLower.Checked then EditMode := EditMode + [emLower] else EditMode := EditMode - [emLower];
    if cbGrow.Checked then EditMode := EditMode + [emGrow] else EditMode := EditMode - [emGrow];

    if cbSelect.Checked then Options := Options + [ocSelect] else Options := Options - [ocSelect];
    if cbFocus.Checked then Options := Options + [ocFocus] else Options := Options - [ocFocus];
    if cbButtons.Checked then Options := Options + [ocButtons] else Options := Options - [ocButtons];
    if cbEdit.Checked then Options := Options + [ocEdit] else Options := Options - [ocEdit];
    if cbCanDrag.Checked then Options := Options + [ocCanDrag] else Options := Options - [ocCanDrag];
    if cbShowDrag.Checked then Options := Options + [ocShowDrag] else Options := Options - [ocShowDrag];
    if cbInsDel.Checked then Options := Options + [ocInsDel] else Options := Options - [ocInsDel];
    if CbShowImages.Checked then Images := MainForm.ilTree else Images := nil;

    IndentX := seX.Value;
    IndentY := seY.Value;
    LineWidth := seLineWidth.Value;
  end;
end;

end.
