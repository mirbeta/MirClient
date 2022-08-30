unit InPlaceEditorsDemoMultiLineText;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, cxControls, cxContainer, ExtCtrls, InPlaceEditorsDemoFrameManager;

type
  TfrmMultiLineTextEditors = class(TEditorDemoBaseFrame)
    RichEdit1: TRichEdit;
    Memo1: TMemo;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure SetParameters(ARichTextStream: TStringStream; APlainTextStream: TStringStream);
  end;

implementation

{$R *.dfm}

{ TfrmMultiLineTextEditors }

procedure TfrmMultiLineTextEditors.SetParameters(ARichTextStream: TStringStream; APlainTextStream: TStringStream);

  procedure LoadText(AStrings: TStrings; AStream: TStream);
  begin
    if AStream <> nil then
    begin
      AStrings.Clear;
      AStrings.LoadFromStream(AStream);
    end;
  end;

begin
  LoadText(RichEdit1.Lines, ARichTextStream);
  LoadText(Memo1.Lines, APlainTextStream);
end;

procedure TfrmMultiLineTextEditors.FormCreate(Sender: TObject);
begin
  Splitter1.Left := Memo1.Left;
end;

end.
