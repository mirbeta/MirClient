unit AdvSmoothTileListEx;

interface

uses
  Classes, AdvSmoothTileList;

type

  TAdvSmoothTileContentEx = class(TAdvSmoothTileContent)
  private
    FExtra: string;
  published
    property Extra: string read FExtra write FExtra;
  end;

  TAdvSmoothTileEx = class(TAdvSmoothTile)
  private
    FExtra: string;
  public
    procedure Assign(Source: TPersistent); override;
    function CreateContent: TAdvSmoothTileContent; override;
    function CreateContentMaximized: TAdvSmoothTileContent; override;
  published
    property Extra: string read FExtra write FExtra;
  end;

  TAdvSmoothTilesEx = class(TAdvSmoothTiles)
  public
    function CreateItemClass: TCollectionItemClass; override;
  end;

  TAdvSmoothTileListEx = class(TAdvSmoothTileList)

  public
    function CreateTiles: TAdvSmoothTiles; override;
  end;

implementation

{ TAdvSmoothTileEx }

procedure TAdvSmoothTileEx.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TAdvSmoothTileEx) then
    FExtra := (Source as TAdvSmoothTileEx).Extra;
end;

function TAdvSmoothTileEx.CreateContent: TAdvSmoothTileContent;
begin
  Result := TAdvSmoothTileContentEx.Create(Self);
end;

function TAdvSmoothTileEx.CreateContentMaximized: TAdvSmoothTileContent;
begin
  Result := TAdvSmoothTileContentEx.Create(Self);
end;

{ TAdvSmoothTilesEx }

function TAdvSmoothTilesEx.CreateItemClass: TCollectionItemClass;
begin
  Result := TAdvSmoothTileEx;
end;

{ TAdvSmoothTileListEx }

function TAdvSmoothTileListEx.CreateTiles: TAdvSmoothTiles;
begin
  Result := TAdvSmoothTilesEx.Create(Self);
end;

end.
