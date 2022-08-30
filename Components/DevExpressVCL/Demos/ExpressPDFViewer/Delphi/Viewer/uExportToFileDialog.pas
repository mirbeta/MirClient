unit uExportToFileDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
{$IFDEF EXPRESSSKINS}
  dxSkinsCore, dxSkinsForm, dxSkinsdxBarPainter, dxSkinscxPCPainter,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, cxGraphics,
  cxControls, cxClasses, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxProgressBar, cxLabel, Menus,
  cxButtons, ComCtrls, ShlObj, cxShellCommon, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxShellComboBox, cxCheckBox,
  cxRadioGroup, cxSpinEdit, dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxLayoutContainer, dxLayoutControl,
  dxLayoutLookAndFeels;

type
  { TfrmExportToFileDialog }

  TfrmExportToFileDialog = class(TForm)
    sePageZoom: TcxSpinEdit;
    btnOk: TButton;
    btnCancel: TButton;
    cbOpenAfterExport: TcxCheckBox;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutControl1: TdxLayoutControl;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutSkinLookAndFeel1: TdxLayoutSkinLookAndFeel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  end;

implementation

{$R *.dfm}

procedure TfrmExportToFileDialog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

end.
