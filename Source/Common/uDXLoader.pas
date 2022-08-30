unit uDXLoader;

interface
  uses Windows, Classes, SysUtils, AsphyreFactory, AsphyreUtils,// Share,
  DX9Providers, DX9Types, AsphyreD3D9,
  DX7Providers, DX7Types, AsphyreD3D7;

function LoadDirectX: Boolean;
procedure PrintScreen(Width, Height: Integer; var AFileName: String);

implementation

function LoadDirectX: Boolean;
begin
  Result := False;
//  if (AsphyreD3D7.DXFileDLL <> 0) and Assigned(DirectXFileCreate) then
//  begin
//    Factory.UseProvider(idDirectX7);
//    Result := True;
//    Exit;
//  end;
  if AsphyreD3D9.LoadDirect3D9 then
  begin
    Factory.UseProvider(idDirectX9);
    Result := True;
    Exit;
  end;
end;

var
  g_nCaptureSerial: Integer = 0;
  g_boPrinting: Boolean = False;

//procedure PrintScreen(Width, Height: Integer; var AFileName: String);
//
//  function IntToStr2(n: Integer): string;
//  begin
//    if n < 10 then
//      Result := '0' + IntToStr(n)
//    else
//      Result := IntToStr(n);
//  end;
//
//  function GetFileName: String;
//  begin
//    Result := '';
//    while True do
//    begin
//      Inc(g_nCaptureSerial);
//      Result := 'Images\Images' + IntToStr2(g_nCaptureSerial) + '.png';
//      if not FileExists(Result) then
//        Break;
//    end;
//  end;
//
//var
//  ASurface: IDirect3DSurface9;
//  S: String;
//begin
//  if g_boPrinting then
//    Exit;
//
//  if not DirectoryExists('Images') then
//    CreateDir('Images');
//
//  if Succeeded(D3D9Device.CreateOffscreenPlainSurface(Width, Height, D3DFMT_A8R8G8B8, D3DPOOL_SCRATCH, ASurface, nil)) then
//  begin
//    AFileName := GetFileName;
//    S := AFileName;
//    g_boPrinting := True;
//    AFileName := 'Images\' + IntToStr2(g_nCaptureSerial) + '.png';
//    if Succeeded(D3D9Device.GetBackBuffer(0, 0, D3DBACKBUFFER_TYPE_MONO, ASurface)) then
//    begin
//      TThread.CreateAnonymousThread(
//        procedure
//        begin
//          try
//            D3DXSaveSurfaceToFile(PChar(S), D3DXIFF_PNG, ASurface, nil, nil);
//            ASurface := nil;
//            g_boPrinting := False;
//          except
//          end;
//        end
//      ).Start;
//    end
//    else if Succeeded(D3D9Device.GetFrontBufferData(0, ASurface)) then
//    begin
//      TThread.CreateAnonymousThread(
//        procedure
//        begin
//          try
//            D3DXSaveSurfaceToFile(PChar(S), D3DXIFF_PNG, ASurface, nil, nil);
//            ASurface := nil;
//            g_boPrinting := False;
//          except
//          end;
//        end
//      ).Start;
//    end;
//  end;
//end;


procedure PrintScreen(Width, Height: Integer; var AFileName: String);

  function IntToStr2(n: Integer): string;
  begin
    if n < 10 then
      Result := '0' + IntToStr(n)
    else
      Result := IntToStr(n);
  end;

  function GetFileName: String;
  begin
    Result := '';
    while True do
    begin
      Inc(g_nCaptureSerial);
      Result := 'Images\Images' + IntToStr2(g_nCaptureSerial) + '.png';
      if not FileExists(Result) then
        Break;
    end;
  end;

var
  ASurface: IDirect3DSurface9;
  S: String;
begin
  if g_boPrinting then
    Exit;

  if not DirectoryExists('Images') then
    CreateDir('Images');

  if Succeeded(D3D9Device.CreateOffscreenPlainSurface(Width, Height, D3DFMT_A8R8G8B8, D3DPOOL_SCRATCH, ASurface, nil)) then
  begin
    AFileName := GetFileName;
    S := AFileName;
    //g_boPrinting := True;
    AFileName := 'Images\' + IntToStr2(g_nCaptureSerial) + '.png';
//    if Succeeded(D3D9Device.GetBackBuffer(0, 0, D3DBACKBUFFER_TYPE_MONO, ASurface)) then
//    begin
//      try
//        D3DXSaveSurfaceToFile(PChar(S), D3DXIFF_PNG, ASurface, nil, nil);
//        ASurface := nil;
//        g_boPrinting := False;
//      except
//      end;
//
//    end
//    else if Succeeded(D3D9Device.GetFrontBufferData(0, ASurface)) then
//    begin
//      try
//        D3DXSaveSurfaceToFile(PChar(S), D3DXIFF_PNG, ASurface, nil, nil);
//        ASurface := nil;
//        g_boPrinting := False;
//      except
//      end;
//    end;

     if Succeeded(D3D9Device.GetFrontBufferData(0, ASurface)) then
     begin
      try
        D3DXSaveSurfaceToFile(PChar(S), D3DXIFF_PNG, ASurface, nil, nil);
        ASurface := nil;
        g_boPrinting := False;
      except
      end;
     end;
  end;
end;

end.
