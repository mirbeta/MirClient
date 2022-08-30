{***************************************************************************}
{ TWebPictureContainer                                                      }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by                                                                }
{   TMS Software                                                            }
{   copyright © 2006 - 2014                                                 }
{   Email : info@tmssoftware.com                                            }
{   Web : http://www.tmssoftware.com                                        }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit WebPictureContainer;

interface

uses
  Classes, PictureContainer, Graphics, AdvGDIP;

type
  TWebPictureContainer = class;

  TDownLoadThread = class(TThread)
  private
    FPictureContainer: TWebPictureContainer;
    FWorkerItem: integer;
    FWorkerPosition: int64;
    FWorkerTotalSize: int64;
  protected
    procedure DoProgress;
    procedure DoComplete;
    procedure Execute; override;
    function Download(URL: string): TMemoryStream;
  public
    constructor Create(APictureContainer: TWebPictureContainer);
  end;

  TWebPictureItem = class(TPictureItem)
  private
    FURL: string;
    FLoaded: boolean;
    procedure SetURL(const Value: string);
    function GetPicture: TGraphic;
    procedure SetPicture(const Value: TGraphic);
  protected
    function CreatePicture: TGraphic; override;
  public
    property Loaded: boolean read FLoaded write FLoaded;
  published
    property URL: string read FURL write SetURL;
    property Picture: TGraphic read GetPicture write SetPicture;
  end;

  TWebPictureCollection = class(TPictureCollection)
  private
    function GetItem(Index: Integer): TWebPictureItem;
    procedure SetItem(Index: Integer; const Value: TWebPictureItem);
  protected
    function GetItemClass: TCollectionItemClass; override;
    function FindPictureByURL(AURL: string): TGraphic;
  public
    function Add: TWebPictureItem;
    property Items[Index: Integer]: TWebPictureItem read GetItem write SetItem; default;
  published
  end;

  TDownloadCompleteEvent = procedure(Sender: TObject; ItemIndex: integer) of object;
  TDownloadProgressEvent = procedure(Sender: TObject; ItemIndex: integer; Position, TotalSize: int64) of object;

  TWebPictureContainer = class(TPictureContainer)
  private
    FDownloader: TDownLoadThread;
    FDefaultPicture: TAdvGDIPPicture;
    FOnDownloadComplete: TDownloadCompleteEvent;
    FOnDownloadProgress: TDownloadProgressEvent;
    function GetItems: TWebPictureCollection;
    procedure SetItems(const Value: TWebPictureCollection);
    procedure SetDefaultPicture(const Value: TAdvGDIPPicture);

  protected
    function CreateItems: TPictureCollection; override;
    procedure DoDownloadComplete(ItemIndex: integer); virtual;
    procedure DoDownloadProgress(ItemIndex: integer; Position,TotalSize: int64); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindPictureByURL(AURL: string): TGraphic;
    function FindPicture(s: string): TGraphic; override;
  published
    property DefaultPicture: TAdvGDIPPicture read FDefaultPicture write SetDefaultPicture;
    property Items: TWebPictureCollection read GetItems write SetItems;
    property OnDownloadComplete: TDownloadCompleteEvent read FOnDownloadComplete write FOnDownloadComplete;
    property OnDownloadProgress: TDownloadProgressEvent read FOnDownloadProgress write FOnDownloadProgress;
  end;


implementation

uses
  Windows, WinInet, SysUtils;

{ TWebPictureCollection }

function TWebPictureCollection.Add: TWebPictureItem;
begin
  Result := TWebPictureItem(inherited Add);
end;

function TWebPictureCollection.FindPictureByURL(AURL: string): TGraphic;
var
  s: string;
  i: Integer;
begin
  Result := nil;
  s := Uppercase(AURL);
  i := 1;
  while i <= Count do
  begin
    if Uppercase(Items[i - 1].URL) = s then
    begin
      Result := Items[i - 1].Picture;
      Break;
    end;
    Inc(i);
  end;
end;

function TWebPictureCollection.GetItem(Index: Integer): TWebPictureItem;
begin
  Result := TWebPictureItem(inherited Items[Index]);
end;

function TWebPictureCollection.GetItemClass: TCollectionItemClass;
begin
  Result := TWebPictureItem;
end;

procedure TWebPictureCollection.SetItem(Index: Integer;
  const Value: TWebPictureItem);
begin
  inherited Items[Index] := Value;
end;

{ TWebPictureContainer }

constructor TWebPictureContainer.Create(AOwner: TComponent);
begin
  inherited;

  FDownloader := nil;
  FDefaultPicture := TAdvGDIPPicture.Create;
  if not (csDesigning in ComponentState) then
  begin
    FDownloader := TDownloadThread.Create(Self);
    FDownloader.FreeOnTerminate := true;
  end;
end;

function TWebPictureContainer.CreateItems: TPictureCollection;
begin
  Result := TWebPictureCollection.Create(Self);
end;

destructor TWebPictureContainer.Destroy;
begin
  FDefaultPicture.Free;
  if Assigned(FDownloader) then
    FDownloader.Terminate;
  inherited;
end;

procedure TWebPictureContainer.DoDownloadComplete(ItemIndex: integer);
begin
  if Assigned(OnDownloadComplete) then
    OnDownloadComplete(Self, ItemIndex);
end;

procedure TWebPictureContainer.DoDownloadProgress(ItemIndex: integer; Position,
  TotalSize: int64);
begin
  if Assigned(OnDownloadProgress) then
    OnDownloadProgress(Self, ItemIndex, Position, TotalSize);
end;

function TWebPictureContainer.FindPicture(s: string): TGraphic;
begin
  Result := inherited FindPicture(s);
  if not Assigned(Result) then
    Result := DefaultPicture;
end;

function TWebPictureContainer.FindPictureByURL(AURL: string): TGraphic;
begin
  Result := Items.FindPictureByURL(AURL)
end;

function TWebPictureContainer.GetItems: TWebPictureCollection;
begin
  Result := TWebPictureCollection(inherited Items);
end;

procedure TWebPictureContainer.SetDefaultPicture(const Value: TAdvGDIPPicture);
begin
  FDefaultPicture.Assign(Value);
end;

procedure TWebPictureContainer.SetItems(const Value: TWebPictureCollection);
begin
  (inherited Items).Assign(Value);
end;

{ TWebPictureItem }

function TWebPictureItem.CreatePicture: TGraphic;
begin
  Result := TAdvGDIPPicture.Create;
end;

function TWebPictureItem.GetPicture: TGraphic;
begin
  if Loaded then
    Result := inherited Picture
  else
    Result := ((Collection as TWebPictureCollection).Owner as TWebPictureContainer).DefaultPicture;
end;

procedure TWebPictureItem.SetPicture(const Value: TGraphic);
begin
  (inherited Picture).Assign(Value);
end;

procedure TWebPictureItem.SetURL(const Value: string);
begin
  if (FURL <> Value) then
  begin
    FURL := Value;
    FLoaded := false;
  end;
end;

{ TDownLoadThread }

constructor TDownLoadThread.Create(APictureContainer: TWebPictureContainer);
begin
  inherited Create(False);
  FPictureContainer := APictureContainer;
 //self.Priority := tpLower;
end;

procedure TDownLoadThread.DoComplete;
begin
  FPictureContainer.DoDownloadComplete(FWorkerItem);
end;

procedure TDownLoadThread.DoProgress;
begin
  FPictureContainer.DoDownloadProgress(FWorkerItem,FWorkerPosition,FWorkerTotalSize);
end;

function TDownLoadThread.Download(URL: string): TMemoryStream;
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
  Result := nil;

  fISession := InternetOpen('WebPictureContainer',INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0);
  if (fISession = nil) then
  begin
    //DownLoadError('Cannot open internet session');
    //fThreadBusy:=false;
    Exit;
  end;

  fIHttp := InternetOpenURL(fISession,pchar(URL),nil,0,
   INTERNET_FLAG_PRAGMA_NOCACHE or INTERNET_FLAG_NO_CACHE_WRITE or INTERNET_FLAG_RELOAD,0);

  if (fIHttp = nil) then
  begin
    InternetCloseHandle(fISession);
    //DownLoadError('Cannot open http connection');
    //fThreadBusy:=false;
    Exit;
  end;

  try
    dwBufSize := SizeOf(cbuf);
    dwidx := 0;
    HttpQueryInfo(fIHttp,HTTP_QUERY_STATUS_CODE,@cbuf,dwBufSize,dwIdx);

    val(cbuf,httpstatus,err);
    if (httpstatus <> 200) or (err <> 0) then
    begin
      InternetCloseHandle(fISession);
      InternetCloseHandle(fIHttp);
      //DownLoadError('Cannot open URL '+furl);
      //FThreadBusy:=false;
      Exit;
    end;

    dwBufSize := SizeOf(cbuf);
    dwidx := 0;
    HttpQueryInfo(fIHttp,HTTP_QUERY_CONTENT_TYPE,@cbuf,dwBufSize,dwIdx);

    if Pos('IMAGE',UpperCase(StrPas(cbuf))) = 0 then
    begin
      InternetCloseHandle(fISession);
      InternetCloseHandle(fIHttp);
      //DownLoadError('Resource is not of image type : ' + FUrl);
      //fThreadBusy := false;
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
      //DownLoadError('Image size is 0');
      //fThreadBusy:=false;
      Exit;
    end;

    //DownLoadProgress(0,httpsize);

    len := 4096;
    RBSIZE := 4096;

    ms := TMemoryStream.Create;

    cancel := false;

    FWorkerTotalSize := httpsize;

    while (len = RBSIZE) and not Cancel do
    begin
      InternetReadFile(fIHttp,@rb,RBSIZE,len);
      if len>0 then ms.WriteBuffer(rb,len);
      //DownLoadProgress(ms.Size,httpsize);
      //DownLoadCancel(cancel);
      FWorkerPosition := ms.Size;
      Synchronize(DoProgress);
    end;

    if not cancel then
    begin
      Result := ms;
      //ms.Position := 0;
      //LoadFromStream(ms);
    end;

      FWorkerPosition := ms.Size;
      Synchronize(DoProgress);
    //ms.Free;
  finally
    InternetCloseHandle(fIHttp);
    InternetCloseHandle(fISession);

    //FThreadBusy:=false;
  end;
end;

procedure TDownLoadThread.Execute;
var
  i: integer;
  ms: TMemoryStream;
begin
  while not Terminated do
  begin
    for I := 0 to FPictureContainer.Items.Count - 1 do
    begin
      if not FPictureContainer.Items[I].Loaded and (FPictureContainer.Items[i].URL <> '') then
      begin
        outputdebugstring(pchar(FPictureContainer.Items[i].URL));

        FWorkerItem := i;
        FWorkerPosition := 0;
        FWorkerTotalSize := 0;
        ms := Download(FPictureContainer.Items[I].URL);

        FPictureContainer.Items[I].Loaded := true;

        ms.Position := 0;
        FPictureContainer.Items[I].Picture.LoadFromStream(ms);

        outputdebugstring(pchar('Done: '+inttostr(ms.Size)));

        Synchronize(DoComplete);

        if Assigned(ms) then
          ms.Free;
      end;
    end;
  end;
end;




end.
