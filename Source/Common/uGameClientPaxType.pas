unit uGameClientPaxType;

//客户端Pax脚本类型
interface

uses
  uGameEngine, AbstractCanvas, AbstractTextures;

type
  TEndRenderProc = procedure(Canvas: TAsphyreCanvas);

var
  OnEnderRender: TEndRenderProc = nil;

procedure DoOnEndRender(Canvas: TAsphyreCanvas);

implementation

procedure DoOnEndRender(Canvas: TAsphyreCanvas);
begin
  if Assigned(OnEnderRender) then begin
    TEndRenderProc(OnEnderRender)(Canvas);
  end;
end;

end.

