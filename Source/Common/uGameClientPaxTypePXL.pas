unit uGameClientPaxTypePXL;

//�ͻ���Pax�ű�����
interface

uses
  uGameEngine, PXL.Canvas, PXL.Textures;

type
  TEndRenderProc = procedure(Canvas: TCustomCanvas);

var
  OnEnderRender: TEndRenderProc = nil;

procedure DoOnEndRender(Canvas: TCustomCanvas);

implementation

procedure DoOnEndRender(Canvas: TCustomCanvas);
begin
  if Assigned(OnEnderRender) then begin
    TEndRenderProc(OnEnderRender)(Canvas);
  end;
end;

end.

