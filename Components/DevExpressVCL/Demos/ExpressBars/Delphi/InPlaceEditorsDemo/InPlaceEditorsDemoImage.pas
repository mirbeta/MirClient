unit InPlaceEditorsDemoImage;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, InPlaceEditorsDemoFrameManager, cxControls, cxContainer,
  cxEdit, cxImage, StdCtrls;

type
  TfrmImageEditors = class(TEditorDemoBaseFrame)
    cxImage1: TcxImage;
  private
    { Private declarations }
  public
    procedure SetParameters(AStream: TStream);
  end;

implementation

{$R *.dfm}

{ TfrmImageEditors }

procedure TfrmImageEditors.SetParameters(AStream: TStream);
begin
  if AStream <> nil then
    cxImage1.Picture.Bitmap.LoadFromStream(AStream)
  else
    cxImage1.Picture.Bitmap := nil;
end;

end.
