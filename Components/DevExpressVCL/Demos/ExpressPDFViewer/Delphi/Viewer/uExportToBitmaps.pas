unit uExportToBitmaps;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxLayoutContainer, cxClasses, cxCheckBox, StdCtrls, cxTextEdit,
  cxMaskEdit, cxSpinEdit, Classes, Controls, dxLayoutControl, uExportToFileDialog;

type
  { TfrmExportToBitmaps }

  TfrmExportToBitmaps = class(TfrmExportToFileDialog)
    teFilePrefix: TcxTextEdit;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    procedure FormCreate(Sender: TObject);

  end;

implementation

{$R *.dfm}

procedure TfrmExportToBitmaps.FormCreate(Sender: TObject);
begin
  inherited;
  cbOpenAfterExport.Caption := 'Open folder after export';
end;

end.
