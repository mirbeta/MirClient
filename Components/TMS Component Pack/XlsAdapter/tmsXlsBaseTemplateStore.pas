unit tmsXlsBaseTemplateStore;
{$INCLUDE ..\FLXCOMPILER.INC}
{$INCLUDE ..\FLXCONFIG.INC}

interface

uses
  SysUtils, Classes,
  Contnrs,
  tmsXlsMessages, tmsUFlxMessages;

type
  TXlsBaseTemplateStore = class(TComponent)
  private
    { Private declarations }
  protected
    function GetStoredFile(Name: UTF16String): ByteArray;virtual;abstract;
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    function IsUpToDate: boolean;virtual; abstract;
    procedure Refresh; virtual; abstract;
    property StoredFile[Name: UTF16String]: ByteArray read GetStoredFile;
    { Public declarations }
  published
    { Published declarations }
  end;

implementation
{ TXlsBaseTemplateStore }

constructor TXlsBaseTemplateStore.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TXlsBaseTemplateStore.Loaded;
begin
  inherited;
end;

end.


