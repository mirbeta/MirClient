const
  HIMETRIC_INCH = 2540;


{ THTMLPicture }

procedure THTMLPicture.Assign(Source: TPersistent);
var
  ms: TMemoryStream;
begin
  FIsEmpty := true;
  gpPicture := nil;
  FFrameCount := -1;
  FNextCount := -1;
  FTimerCount := -1;
  Frame := 1;

  if (Source = nil) then
    FDataStream.Clear
  else
  begin
    if (Source is THTMLPicture) then
    begin
      FStretched := (Source as THTMLPicture).Stretch;
      FFrame := (Source as THTMLPicture).Frame;
      FID := (Source as THTMLPicture).ID;
      FDataStream.LoadFromStream(THTMLPicture(Source).fDataStream);
      FIsEmpty := False;
      LoadPicture;
      if Assigned(OnChange) then
        OnChange(self);
    end;
    if (Source is TPicture) then
    begin
      ms := TMemoryStream.Create;
      (Source as TPicture).Graphic.SaveToStream(ms);
      ms.Position := 0;
      LoadFromStream(ms);
      ms.Free;
    end;
  end;    
end;

constructor THTMLPicture.Create;
begin
  inherited;
  FDataStream := TMemoryStream.Create;
  FIsEmpty := True;
  gpPicture := nil;
  FLogPixX := 96;
  FLogPixY := 96;
  FThreadBusy := False;
  FAsynch := True;
  FFrameCount := -1;
  FNextCount := -1;
  FTimerCount := -1;
  FFrame := 1;
  FIsDB := False;
end;

destructor THTMLPicture.Destroy;
begin
  FDataStream.Free;
  inherited;
end;

procedure THTMLPicture.LoadPicture;
const
  IID_IPicture: TGUID = (
  D1:$7BF80980;D2:$BF32;D3:$101A;D4:($8B,$BB,$00,$AA,$00,$30,$0C,$AB));

var
  hGlobal: THandle;
  pvData: Pointer;
  pstm: IStream;
  hr: hResult;
  GifStream: TMemoryStream;
  i: Integer;
  b,c,d,e: Byte;
  skipimg: Boolean;
  imgidx: Integer;
begin
  hGlobal := GlobalAlloc(GMEM_MOVEABLE, FDataStream.Size);
  if hGlobal = 0 then
    raise Exception.Create('Could not allocate memory for image');

  pvData := GlobalLock(hGlobal);
  FDataStream.Position := 0;

  FFrameXPos := 0;
  FFrameYPos := 0;
  FAnimMaxX := 0;
  FAnimMaxY := 0;

  {skip first image ctrl}

  if IsGIF and (FrameCount > 0) then
   begin
    //manipulate the stream here for animated GIF ?
    Gifstream := TMemoryStream.Create;

    ImgIdx := 1;
    SkipImg := False;

    FDataStream.Position := 6;
    FDataStream.Read(FAnimMaxX,2);
    FDataStream.Read(FAnimMaxY,2);

    for i := 1 to FDataStream.Size do
     begin
       FDataStream.Position := i - 1;
       FDataStream.Read(b,1);

       if (b = $21) and (i + 8 < FDataStream.Size) then
        begin
         FDataStream.Read(c,1);
         FDataStream.Read(d,1);
         FDataStream.Position := FDataStream.Position + 5;

         FDataStream.Read(e,1);
         if (c = $F9) and (d = $4) and (e = $2C) then
           begin
             if imgidx = FFrame then
              begin
               FDataStream.Read(FFrameXPos,2);
               FDataStream.Read(FFrameYPos,2);
               FDataStream.Read(FFrameXSize,2);
               FDataStream.Read(FFrameYSize,2);
              end;

             Inc(ImgIdx);
             if ImgIdx <= FFrame then
               SkipImg := True
             else
               SkipImg := False;
           end;
        end;
      if not SkipImg then GifStream.Write(b,1);
     end;
    GifStream.Position := 0;
    GifStream.ReadBuffer(pvData^,GifStream.Size);
    GifStream.Free;
   end
  else
  begin
    FDataStream.ReadBuffer(pvData^,fDataStream.Size);
  end;

  GlobalUnlock(hGlobal);

  pstm := nil;

  // Create IStream* from global memory
  hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);

  if not (hr = S_OK) then
  begin
    GlobalFree(hGlobal);
    raise Exception.Create('Could not create image stream');
  end
  else
    if (pstm = nil) then
      raise Exception.Create('Empty image stream created');

  // Create IPicture from image file
  hr := OleLoadPicture(pstm, FDataStream.Size,TRUE,IID_IPicture,gpPicture);

  if not (hr = S_OK) then
    raise Exception.Create('Could not load image. Invalid format')
  else
    if gpPicture = nil then
      raise Exception.Create('Could not load image');
end;

procedure THTMLPicture.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  hmWidth:integer;
  hmHeight:integer;
  nPixX,nPixY:integer;
  pnWidth,pnHeight:integer;

begin
  if Empty then Exit;

  if gpPicture = nil then Exit;

  hmWidth  := 0;
  hmHeight := 0;
  gpPicture.get_Width(hmWidth);
  gpPicture.get_Height(hmHeight);

  if Stretch then
  begin
    gpPicture.Render(ACanvas.Handle,Rect.Left,Rect.Bottom,Rect.Right - Rect.Left,-(Rect.Bottom - Rect.Top),0,0,
                     hmWidth,hmHeight, Rect);
  end
  else
  begin
    nPixX := GetDeviceCaps(ACanvas.Handle,LOGPIXELSX);
    nPixY := GetDeviceCaps(ACanvas.Handle,LOGPIXELSY);
    //Convert to device units
    pnWidth  := MulDiv(hmWidth,  nPixX, HIMETRIC_INCH);
    pnHeight := MulDiv(hmHeight, nPixY, HIMETRIC_INCH);

    //gpPicture.Render(ACanvas.Handle,Rect.Left,Rect.Top + pnHeight,pnWidth,-pnHeight,0,0,
    //                 hmWidth,hmHeight, Rect);
    gpPicture.Render(ACanvas.Handle,Rect.Left,Rect.Top,
      pnWidth,pnHeight,0,hmHeight, hmWidth,-hmHeight, Rect);
  end;

end;

function THTMLPicture.GetEmpty: Boolean;
begin
  Result := FIsEmpty;
end;

function THTMLPicture.GetHeight: integer;
var
  hmHeight:integer;
begin
  if gpPicture = nil then
    Result := 0
  else
  begin
    gpPicture.get_Height(hmHeight);
    Result := MulDiv(hmHeight, FLogPixY, HIMETRIC_INCH);
  end;
end;

function THTMLPicture.GetWidth: Integer;
var
  hmWidth: Integer;
begin
  if gpPicture = nil then
    Result := 0
  else
  begin
    gpPicture.get_Width(hmWidth);
    Result := MulDiv(hmWidth, FLogPixX, HIMETRIC_INCH);
  end;
end;

procedure THTMLPicture.LoadFromFile(const FileName: string);
begin
  try
    FDataStream.LoadFromFile(Filename);
    FIsEmpty:=false;
    LoadPicture;
    if Assigned(OnChange) then
      OnChange(self);
  except
    FIsEmpty:=true;
  end;
end;

procedure THTMLPicture.LoadFromStream(Stream: TStream);
begin
  if Assigned(Stream) then
  begin
    FDataStream.LoadFromStream(Stream);
    FIsEmpty := False;
    LoadPicture;
    if Assigned(OnChange) then
      OnChange(self);
  end;
end;

procedure THTMLPicture.ReadData(Stream: TStream);
begin

 if assigned(Stream) then
   begin
     fDataStream.LoadFromStream(stream);
     fIsEmpty:=false;
     LoadPicture;
   end;
end;

procedure THTMLPicture.SaveToStream(Stream: TStream);
begin
  if Assigned(Stream) then fDataStream.SaveToStream(Stream);
end;

procedure THTMLPicture.LoadFromResourceName(Instance: THandle; const ResName: string);
var
  Stream: TCustomMemoryStream;
begin
  if FindResource(Instance,pchar(ResName),RT_RCDATA)<>0 then
  begin
    Stream := TResourceStream.Create(Instance, ResName, RT_RCDATA);
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;

procedure THTMLPicture.LoadFromResourceID(Instance: THandle; ResID: Integer);
var
  Stream: TCustomMemoryStream;
begin
  Stream := TResourceStream.CreateFromID(Instance, ResID, RT_RCDATA);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;


procedure THTMLPicture.SetHeight(Value: integer);
begin

end;

procedure THTMLPicture.SetWidth(Value: integer);
begin

end;

procedure THTMLPicture.WriteData(Stream: TStream);
begin
  if Assigned(Stream) then
   begin
     FDataStream.savetostream(stream);
   end;
end;

procedure THTMLPicture.LoadFromURL(url: string);
var
  UUrl: string;
begin
  UUrl := UpperCase(url);

  if Pos('RES://',UUrl) = 1 then
  begin
    ID := url;
    Delete(url,1,6);
    if url <> '' then
      LoadFromResourceName(hinstance,url);
    Exit;
  end;

  if Pos('FILE://',Uurl) = 1 then
  begin
    ID := url;
    Delete(url,1,7);
    if url <> '' then
      LoadFromFile(url);
    Exit;
  end;

  if FAsynch then
  begin
    if FThreadBusy then
      Exit;
    FURL := url;
    FThreadBusy := True;
    TDownLoadThread.Create(self);
  end
  else
  begin
    FURL := url;
    ID := url;
    {$IFDEF USEWININET}
    DownLoad;
    {$ENDIF}
  end;
end;

{$IFDEF USEWININET}
procedure THTMLPicture.DownLoad;
var
  RBSIZE:dword;
  httpstatus,httpsize,err:integer;
  dwIdx:dword;
  dwBufSize:dword;
  ms:TMemoryStream;
  len:dword;
  cbuf:array[0..255] of char;
  rb:array[0..4095] of byte;

  FISession:hinternet;
  FIHttp:hinternet;
  Cancel:boolean;

begin
  fISession:=InternetOpen('HTMLImage',INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0);
  if (fISession=nil) then
  begin
    DownLoadError('Cannot open internet session');
    fThreadBusy:=false;
    Exit;
  end;

  fIHttp:=InternetOpenURL(fISession,pchar(furl),nil,0,
   INTERNET_FLAG_PRAGMA_NOCACHE or INTERNET_FLAG_NO_CACHE_WRITE or INTERNET_FLAG_RELOAD,0);

  if (fIHttp=nil) then
  begin
    InternetCloseHandle(fISession);
    DownLoadError('Cannot open http connection');
    fThreadBusy:=false;
    Exit;
  end;

  dwBufSize := SizeOf(cbuf);
  dwidx := 0;
  HttpQueryInfo(fIHttp,HTTP_QUERY_STATUS_CODE,@cbuf,dwBufSize,dwIdx);

  val(cbuf,httpstatus,err);
  if (httpstatus <> 200) or (err <> 0) then
  begin
    InternetCloseHandle(fISession);
    InternetCloseHandle(fIHttp);
    DownLoadError('Cannot open URL '+furl);
    FThreadBusy:=false;
    Exit;
  end;

  dwBufSize := SizeOf(cbuf);
  dwidx := 0;
  HttpQueryInfo(fIHttp,HTTP_QUERY_CONTENT_TYPE,@cbuf,dwBufSize,dwIdx);

  if Pos('IMAGE',UpperCase(StrPas(cbuf))) = 0 then
  begin
    InternetCloseHandle(fISession);
    InternetCloseHandle(fIHttp);
    DownLoadError('Resource is not of image type : ' + FUrl);
    fThreadBusy := false;
    Exit;
  end;

  dwBufSize := SizeOf(cbuf);
  dwidx := 0;
  HttpQueryInfo(fIHttp,HTTP_QUERY_CONTENT_LENGTH,@cbuf,dwBufSize,dwIdx);

  val(cbuf,httpsize,err);
  if (httpsize = 0) or (err <> 0) then
  begin
    InternetCloseHandle(fISession);
    InternetCloseHandle(fIHttp);
    DownLoadError('Image size is 0');
    fThreadBusy:=false;
    Exit;
  end;

  DownLoadProgress(0,httpsize);

  len := 4096;
  RBSIZE := 4096;

  ms := TMemoryStream.Create;

  cancel:=false;

  while (len=RBSIZE) and not Cancel do
  begin
    InternetReadFile(fIHttp,@rb,RBSIZE,len);
    if len>0 then ms.WriteBuffer(rb,len);
    DownLoadProgress(ms.Size,httpsize);
    DownLoadCancel(cancel);
  end;

  if not cancel then
  begin
    ms.Position := 0;
    LoadFromStream(ms);
  end;

  ms.Free;

  InternetCloseHandle(fIHttp);
  InternetCloseHandle(fISession);
  FThreadBusy:=false;
end;
{$ENDIF}

procedure THTMLPicture.DownLoadCancel(var cancel: boolean);
begin
  if assigned(FOnDownLoadCancel) then
    FOnDownLoadCancel(self,cancel);
end;

procedure THTMLPicture.DownLoadComplete;
begin
  if Assigned(FOnDownLoadComplete) then
    FOnDownLoadComplete(self);
end;

procedure THTMLPicture.DownLoadError(err: string);
begin
  if Assigned(fOnDownloadError) then
    FOnDownLoadError(self,err);
end;

procedure THTMLPicture.DownLoadProgress(dwSize, dwTotSize: dword);
begin
  if Assigned(FOnDownLoadProgress) then
    FOnDownLoadProgress(self,dwSize,dwTotSize);
end;


procedure THTMLPicture.LoadFromClipboardFormat(AFormat: Word;
  AData: THandle; APalette: HPALETTE);
begin
end;

procedure THTMLPicture.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
end;

function THTMLPicture.GetFrameCount: Integer;
var
  i: Integer;
  b,c,d,e: Byte;
  Res: Integer;
begin
  Result := -1;

  if FFrameCount <> -1 then
    Result := FFrameCount
  else
    if IsGIFFile then
    begin
      Res := 0;
      for i := 1 to FDataStream.Size do
      begin
        FDataStream.Position := i - 1;
        FDataStream.Read(b,1);
        if (b = $21) and (i + 8 < FDataStream.Size) then
        begin
          FDataStream.Read(c,1);
          FDataStream.Read(d,1);
          FDataStream.Position := FDataStream.Position+5;
          FDataStream.Read(e,1);
          if (c = $F9) and (d = $4) and (e = $2C) then Inc(res);
        end;
      end;
      FFrameCount := Res;
      Result := Res;
      FDataStream.Position := 0;
    end;
end;

function THTMLPicture.IsGIFFile: Boolean;
var
  buf: array[0..4] of char;
begin
  Result := False;
  if FDataStream.Size>4 then
  begin
    FDataStream.Position := 0;
    FDataStream.Read(buf,4);
    buf[4] := #0;
    Result := Strpas(buf) = 'GIF8';
    FDataStream.Position := 0;

  end;
end;

function THTMLPicture.GetFrameTime(i: Integer): Integer;
var
 j: Integer;
 b,c,d,e: Byte;
 res: Integer;
 ft: Word;

begin
  Result := -1;

  if IsGIFFile then
  begin
    Res := 0;
    for j := 1 to FDataStream.Size do
    begin
      FDataStream.Position := j-1;
      FDataStream.Read(b,1);
      if (b = $21) and (i + 8 < FDataStream.Size) then
      begin
        FDataStream.Read(c,1);
        FDataStream.Read(d,1);
        FDataStream.Read(b,1);
        {transp. flag here}

        FDataStream.Read(ft,2);
        FDataStream.Position := FDataStream.Position + 2;

        FDataStream.Read(e,1);
        if (c = $F9) and (d = $4) and (e = $2C) then
        begin
          Inc(res);
          if res = i then
          begin
            Result := ft;
            FFrameTransp := b and $01=$01;
            FFrameDisposal := (b shr 3) and $7;
          end;
        end;
      end;
    end;
  end;
  FDataStream.Position := 0;
end;

function THTMLPicture.GetMaxHeight: Integer;
var
  hmHeight: Integer;
begin
  if gpPicture = nil then
    Result := 0
  else
  begin
    if FAnimMaxY>0 then Result:=FAnimMaxY
    else
    begin
      gpPicture.get_Height(hmHeight);
      Result := MulDiv(hmHeight, fLogPixY, HIMETRIC_INCH);
    end;
  end;
end;

function THTMLPicture.GetMaxWidth: Integer;
var
  hmWidth: Integer;
begin
  if gpPicture = nil then
    Result := 0
  else
  begin
    if FAnimMaxX > 0 then
      Result := FAnimMaxX
    else
    begin
      gpPicture.get_Width(hmWidth);
      Result := MulDiv(hmWidth, fLogPixX, HIMETRIC_INCH);
    end;
  end;
end;


procedure THTMLPicture.SetFrame(const Value: Integer);
begin
  FFrame := Value;
  if FDataStream.Size > 0 then
  begin
    LoadPicture;
    if Assigned(OnFrameChange) then
      OnFrameChange(self);
  end;
end;

procedure THTMLPicture.FrameNext;
begin
  if FFrame < FFrameCount then
    Inc(FFrame)
  else
    FFrame := 1;
end;

function THTMLPicture.Step: Boolean;
begin
  Result := False;
  if (FFrameCount <= 1) or FIsEmpty then
    Exit;

  if FNextCount = -1 then
    FrameTime[FFrame];

  if FTimerCount*10 >= FNextCount then
  begin
    FrameNext;
    LoadPicture;
    FNextCount := FNextCount + FrameTime[FFrame];
    Result := True;
  end;

  Inc(FTimerCount);
end;

procedure THTMLPicture.FramePrev;
begin
  if FFrame > 1 then
    Dec(FFrame)
  else
    FFrame := FFrameCount;
end;

function THTMLPicture.GetStretched: boolean;
begin
  Result := FStretched;
end;

procedure THTMLPicture.SetStretched(const Value: boolean);
begin
  FStretched := Value;
end;

{ THTMLImage }

constructor THTMLImage.Create(aOwner: TComponent);
begin
  inherited;
  fHTMLPicture:=THTMLPicture.Create;
  fHTMLPicture.OnChange:=PictureChanged;
  Width:=100;
  Height:=100;
  fHTMLPicture.OnDownLoadError:=DownLoadError;
  fHTMLPicture.OnDownLoadCancel:=DownLoadCancel;
  fHTMLPicture.OnDownLoadProgress:=DownLoadProgress;
  fHTMLPicture.OnDownLoadComplete:=DownLoadComplete;
end;

destructor THTMLImage.Destroy;
begin
  fHTMLPicture.Free;
  inherited;
end;

procedure THTMLImage.Loaded;
begin
  inherited;
  fHTMLPicture.fLogPixX := GetDeviceCaps(canvas.handle,LOGPIXELSX);
  fHTMLPicture.fLogPixY := GetDeviceCaps(canvas.handle,LOGPIXELSY);
end;

procedure THTMLImage.Paint;
var
 xo,yo:integer;

   function Max(a,b:integer):integer;
   begin
    if (a>b) then result:=a else result:=b;
   end;

begin
  inherited;
  if assigned(fHTMLPicture) then
  begin
   if not fHTMLPicture.Empty then
   case fPicturePosition of
   bpTopLeft:Canvas.Draw(0,0,fHTMLPicture);
   bpTopRight:Canvas.Draw(Max(0,width-fHTMLPicture.Width),0,fHTMLPicture);
   bpBottomLeft:Canvas.Draw(0,Max(0,height-fHTMLPicture.Height),fHTMLPicture);
   bpBottomRight:Canvas.Draw(Max(0,width-fHTMLPicture.Width),Max(0,height-fHTMLPicture.Height),fHTMLPicture);
   bpCenter:Canvas.Draw(Max(0,width-fHTMLPicture.Width) shr 1,Max(0,height-fHTMLPicture.Height) shr 1,fHTMLPicture);
   bpTiled:begin
            yo:=0;
            while (yo<Height) do
             begin
              xo:=0;
              while (xo<Width) do
               begin
                Canvas.Draw(xo,yo,fHTMLPicture);
                xo:=xo+fHTMLPicture.Width;
               end;
              yo:=yo+fHTMLPicture.Height;
             end;
           end;
   bpStretched:canvas.StretchDraw(rect(0,0,width,height),fHTMLPicture) else
   end;
  end;

end;

procedure THTMLImage.PictureChanged(sender: TObject);
begin
 Invalidate;
end;

procedure THTMLImage.SetHTMLPicture(const Value: THTMLPicture);
begin
  FHTMLPicture.Assign(Value);
  Invalidate;
end;

procedure THTMLImage.SetPicturePosition(const Value: TPicturePosition);
begin
 if ( fPicturePosition <> Value) then
  begin
   fPicturePosition := Value;
   Invalidate;
  end;
end;

procedure THTMLImage.DownLoadCancel(Sender: TObject; var cancel: boolean);
begin
 if assigned(fOnDownLoadCancel) then fOnDownLoadCancel(self,cancel);
end;

procedure THTMLImage.DownLoadComplete(Sender: TObject);
begin
 if assigned(fOnDownLoadComplete) then fOnDownLoadComplete(self);
end;

procedure THTMLImage.DownLoadError(Sender: TObject; err: string);
begin
  if Assigned(FOnDownloadError) then
    FOnDownLoadError(self,err);
end;

procedure THTMLImage.DownLoadProgress(Sender: TObject; dwSize,
  dwTotSize: dword);
begin
 if Assigned(FOnDownLoadProgress) then
   FOnDownLoadProgress(self,dwSize,dwTotSize);
end;

{ TDownLoadThread }          

constructor TDownLoadThread.Create(aHTMLPicture: THTMLPicture);
begin
  inherited Create(false);
  HTMLPicture := aHTMLPicture;
  FreeOnTerminate := True;
end;

procedure TDownLoadThread.Execute;
begin
 {$IFDEF USEWININET}
 HTMLPicture.DownLoad;
 {$ENDIF}
end;

{ THTMLPictureCache }

destructor THTMLPictureCache.Destroy;
begin
  ClearPictures;
  inherited;
end;

function THTMLPictureCache.AddPicture: THTMLPicture;
begin
  Result := THTMLPicture.Create;
  Add(pointer(result));
end;

procedure THTMLPictureCache.ClearPictures;
var
  i: Integer;
begin
  for i := 1 to Count do
    Items[i - 1].Free;

  Clear;
  //inherited;
end;

function THTMLPictureCache.FindPicture(ID: string): THTMLPicture;
var
  i: Integer;
begin
  Result := nil;
  for i := 1 to Count do
  begin
    if (Items[i - 1].ID = ID) then
    begin
      Result := Items[i - 1];
      Break;
    end;
  end;
end;

function THTMLPictureCache.GetPicture(Index: Integer): THTMLPicture;
begin
  Result := THTMLPicture(inherited Items[Index]);
end;

procedure THTMLPictureCache.SetPicture(Index: Integer; Value: THTMLPicture);
begin
  inherited Items[index] := Pointer(Value);
end;

function THTMLPictureCache.Animate: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Count do
  begin
    if Items[i - 1].Step then
      Result := True;
  end;
end;
