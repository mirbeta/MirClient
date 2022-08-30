{**************************************************************************}
{ TGDIPPictureContainer component                                          }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 2008 - 2012                                                }
{   Email : info@tmssoftware.com                                           }
{   Web : http://www.tmssoftware.com                                       }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit GDIPPictureContainer;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComObj, Activex, WinInet, AdvGDIP;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  //version history
  //v1.0.0.0 : First Release

type
  TPictureItem = class(TCollectionItem)
  private
    FPicture: TAdvGDIPPicture;
    FTag: Integer;
    FName: string;
    procedure SetPicture(const Value: TAdvGDIPPicture);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Picture: TAdvGDIPPicture read FPicture write SetPicture;
    property Name: string read FName write FName;
    property Tag: Integer read FTag write FTag;
  end;

  TPictureCollection = class(TCollection)
  private
    FOwner: TComponent;
    function GetItem(Index: Integer): TPictureItem;
    procedure SetItem(Index: Integer; Value: TPictureItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner:TComponent);
    function Add: TPictureItem;
    function Insert(index:integer): TPictureItem;
    property Items[Index: Integer]: TPictureItem read GetItem write SetItem; default;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TGDIPPictureContainer = class(TComponent)
  private
    FItems: TPictureCollection;
    procedure SetItems(const Value: TPictureCollection);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindPicture(s: string): TAdvGDIPPicture; virtual;
    procedure AddFromResource(ResourceName, PictureName: string);
  published
    { Published declarations }
    property Items: TPictureCollection read FItems write SetItems;
    property Version: string read GetVersion write SetVersion;
  end;

  TAdvGDIPPictureCache = class(TList)
  private
    procedure SetPicture(Index: Integer; Value: TAdvGDIPPicture);
    function GetPicture(Index: Integer):TAdvGDIPPicture;
  public
    destructor Destroy; override;
    property Items[index: Integer]: TAdvGDIPPicture read GetPicture write SetPicture; default;
    function AddPicture: TAdvGDIPPicture;
    function FindPicture(ID:string): TAdvGDIPPicture;
    procedure ClearPictures;
  end;


implementation



{ TPictureItem }

procedure TPictureItem.Assign(Source: TPersistent);
begin
  Name := (Source as TPictureItem).Name;
  Tag := (Source as TPictureItem).Tag;
  Picture.Assign((Source as TPictureItem).Picture)
end;

constructor TPictureItem.Create(Collection: TCollection);
begin
  inherited;
  FPicture := TAdvGDIPPicture.Create;
end;

destructor TPictureItem.Destroy;
begin
  FPicture.Free;
  inherited;
end;

procedure TPictureItem.SetPicture(const Value: TAdvGDIPPicture);
begin
  FPicture.Assign(Value);
end;

{ TPictureCollection }

function TPictureCollection.Add: TPictureItem;
begin
  Result := TPictureItem(inherited Add);
end;

constructor TPictureCollection.Create(AOwner: TComponent);
begin
  inherited Create(TPictureItem);
  FOwner := AOwner;
end;

function TPictureCollection.GetItem(Index: Integer): TPictureItem;
begin
  Result := TPictureItem(inherited Items[Index]);
end;

function TPictureCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TPictureCollection.Insert(index: Integer): TPictureItem;
begin
  Result := TPictureItem(inherited Insert(Index));
end;

procedure TPictureCollection.SetItem(Index: Integer;
  Value: TPictureItem);
begin
  inherited SetItem(Index, Value);
end;

{ TGDIPPictureContainer }

procedure TGDIPPictureContainer.AddFromResource(ResourceName,
  PictureName: string);
var
  pici: TPictureItem;
begin
  pici := Items.Add;
  pici.Picture.LoadFromResourceName(HInstance,ResourceName);
  pici.Name := PictureName;
end;

constructor TGDIPPictureContainer.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TPictureCollection.Create(Self);
end;

destructor TGDIPPictureContainer.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TGDIPPictureContainer.FindPicture(s: string): TAdvGDIPPicture;
var
  i: Integer;
begin
  Result := nil;
  s := Uppercase(s);
  i := 1;
  while i <= Items.Count do
  begin
    if Uppercase(Items.Items[i - 1].Name) = s then
    begin
      Result := Items.Items[i - 1].Picture;
      Break;
    end;
    Inc(i);
  end;
end;

procedure TGDIPPictureContainer.SetItems(const Value: TPictureCollection);
begin
  FItems := Value;
end;

function TGDIPPictureContainer.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TGDIPPictureContainer.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TGDIPPictureContainer.SetVersion(const Value: string);
begin

end;


{ THTMLPictureCache }

destructor TAdvGDIPPictureCache.Destroy;
begin
  ClearPictures;
  inherited;
end;

function TAdvGDIPPictureCache.AddPicture: TAdvGDIPPicture;
begin
  Result := TAdvGDIPPicture.Create;
  Add(pointer(result));
end;

procedure TAdvGDIPPictureCache.ClearPictures;
var
  i: Integer;
begin
  for i := 1 to Count do
    Items[i - 1].Free;
  Clear;
  //inherited;
end;

function TAdvGDIPPictureCache.FindPicture(ID: string): TAdvGDIPPicture;
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

function TAdvGDIPPictureCache.GetPicture(Index: Integer): TAdvGDIPPicture;
begin
  Result := TAdvGDIPPicture(inherited Items[Index]);
end;

procedure TAdvGDIPPictureCache.SetPicture(Index: Integer; Value: TAdvGDIPPicture);
begin
  inherited Items[index] := Pointer(Value);
end;




end.
