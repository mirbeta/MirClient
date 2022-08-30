{*************************************************************************}
{ TImageItem Class                                                        }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2010                                             }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit GDIPPreviewItem;

interface

{$I TMSDEFS.INC}

{$IFDEF BCB}
{$HPPEMIT '#include <shobjidl.h>'}
{$ENDIF}

uses
  Forms, Classes, Controls, Windows, Messages, ShlObj, Graphics,
  SysUtils, ComObj, ActiveX, GDIPCustomItem, GDIPImageItem, ShellApi;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  //v0.9.0.0 : First Beta Release
  //v1.0.0.0 : First Release

type
  TPreviewItem = class(TImageItem)
  private
    fMalloc: IMalloc;
    fRunnableTask: IRunnableTask;
    FPreviewFile: string;
    procedure SetPreviewFile(const Value: string);
  protected
    function GetVersionNr: integer; override;
    function GetFileThumbnail(const FileName: string; out bmp: TBitmap;
      out ErrorInfo: string): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //must override
    function CreateNewItem(AOwner: TComponent): TCustomItem; override;
    function GetClassType: TComponentClass; override;
    class function CustomClassName: String; override;
    procedure Run;
  published
    property PreviewFile: string read FPreviewFile write SetPreviewFile;
  end;

procedure Register;

implementation

function TPreviewItem.GetFileThumbnail(const FileName: string; out bmp: TBitmap;
  out ErrorInfo: string): boolean;
  function GetExtImg(const FileName: string; out ExtImg: IExtractImage): boolean;
  var
    Desktop, Target: IShellFolder;
    PIDL: PItemIDList;
    Attr, Eaten: DWORD;
    res: HResult;
  begin
    OleCheck(SHGetMalloc(fMalloc));
    OleCheck(SHGetDesktopFolder(Desktop));
    OleCheck(Desktop.ParseDisplayName(0, nil,
      PChar(ExcludeTrailingPathDelimiter(ExtractFilePath(FileName))), Eaten,
      PIDL, Attr));
    try
      OleCheck(Desktop.BindToObject(PIDL, nil, IShellFolder, Target));
    finally
      fMalloc.Free(PIDL);
    end;
    OleCheck(Target.ParseDisplayName(0, nil, PChar(ExtractFileName(FileName)),
      Eaten, PIDL, Attr));

    try
      res := Target.GetUIObjectOf(Application.Handle, 1, PIDL, IExtractImage, nil, ExtImg);
      Result := S_OK = res;
      if not Result then
      begin
        if res = E_NOTIMPL then
          ErrorInfo := 'Thumbnail not available for this file type'
        else
          ErrorInfo := Format('GetExtImg failed: %s', [SysErrorMessage(res)]);
      end;
    finally
      fMalloc.Free(PIDL);
    end;
  end;

  function GetThumbnailBmp(const ExtImg: IExtractImage): TBitmap;
  var
    Size: TSize;
    Buf: array [0 .. MAX_PATH] of Char;
    BmpHandle: HBITMAP;
    Prio, Flags: DWORD;
    hres: HResult;
  begin
    Result := nil;
    fRunnableTask := nil;
    Size.cx := ImageWidth; Size.cy := ImageHeight;
    Prio := IEIT_PRIORITY_NORMAL;
    Flags := IEIFLAG_ASYNC or IEIFLAG_QUALITY or IEIFLAG_SCREEN;
    hres := ExtImg.GetLocation(Buf, SizeOf(Buf), Prio, Size, 32, Flags);
    if hres = E_PENDING then
      { If QueryInterface for IRunnableTask succeed, the fRunnableTask
        interface can be used to abort later the running extraction process. }
      if Failed(ExtImg.QueryInterface(IRunnableTask, fRunnableTask)) then
        fRunnableTask := nil;
    if (hres = S_OK) or (hres = E_PENDING) then
    begin
      Result := TBitmap.Create;
      try
        { The call "Extract" consumes a long time depending on its state in the
          Thumbs.db }
        OleCheck(ExtImg.Extract(BmpHandle));
        Result.Handle := BmpHandle;
      except
        on E: Exception do
        begin
          ErrorInfo := Format('Extract failed: %s', [E.Message]);
          FreeAndNil(Result);
        end;
      end;
    end else
      ErrorInfo := Format('GetThumbnailBmp failed: %s', [SysErrorMessage(hres)]);
  end;

  function IsWindowsVistaOrHigher: Boolean;
  var
    VerInfo: TOSVersioninfo;
  begin
    VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
    GetVersionEx(VerInfo);
    Result := VerInfo.dwMajorVersion >= 6;
  end;

  function GetVistaThumbnail(const FileName: string): TBitmap;
  var
    siif: IShellItemImageFactory;
    si2: IShellItem2;
    BmpHandle: HBITMAP;
    Size: TSize;
  begin
    Size.cx := ImageWidth; Size.cy := ImageHeight;
    OleCheck(SHCreateItemFromParsingName(PChar(FileName), nil, IShellItem2, si2));
    OleCheck(si2.QueryInterface(IID_IShellItemImageFactory, siif));
    Result := TBitmap.Create;
    try
      OleCheck(siif.GetImage(Size, SIIGBF_RESIZETOFIT or SIIGBF_THUMBNAILONLY,
        BmpHandle));
      result.Handle := BmpHandle;
    except
      on E: Exception do
      begin
        ErrorInfo := Format('GetVistaThumbnail failed: %s', [E.Message]);
        FreeAndNil(result);
      end;
    end;
  end;

var
  ExtImg: IExtractImage;
begin
  ErrorInfo := '';
  bmp := nil;
  try
//    if IsWindowsVistaOrHigher then
//      bmp := GetVistaThumbnail(FileName)
    if GetExtImg(FileName, ExtImg) then
      bmp := GetThumbnailBmp(ExtImg);
  except
    on E: Exception do
      ErrorInfo := Format('GetFileThumbnail failed: %s', [E.Message]);
  end;
  Result := bmp <> nil;
end;

procedure Register;
begin
  RegisterPolyItem(TPreviewItem);
end;

{ TPreviewItem }

constructor TPreviewItem.Create(AOwner: TComponent);
begin
  inherited;
end;

function TPreviewItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := TPreviewItem.Create(AOwner);
end;

class function TPreviewItem.CustomClassName: String;
begin
  Result := 'Normal Preview Item';
end;

destructor TPreviewItem.Destroy;
begin
  inherited;
end;

function TPreviewItem.GetClassType: TComponentClass;
begin
  Result := TPreviewItem;
end;

function TPreviewItem.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TPreviewItem.Run;
begin
  if FileExists(PreviewFile) then
    ShellExecute(0, 'open', PChar(PreviewFile), nil, nil, SW_SHOWNORMAL);
end;

procedure TPreviewItem.SetPreviewFile(const Value: string);
var
  bmp: TBitmap;
  error: string;
begin
  FPreviewFile := Value;
  if FileExists(FPreviewFile) then
  begin
    GetFileThumbnail(FPreviewFile, bmp, error);
    if Assigned(bmp) then
    begin
      Image.Assign(bmp);
      bmp.Free;
    end;
  end;
  Changed;
end;

end.
