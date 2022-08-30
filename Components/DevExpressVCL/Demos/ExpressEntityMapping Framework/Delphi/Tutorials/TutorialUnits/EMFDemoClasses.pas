unit EMFDemoClasses;

interface

uses
  SysUtils, Classes, Generics.Collections, dxEMF.Attributes, dxEMF.Linq, dxEMF.Linq.Expressions, dxEMF.Core,
  dxEMF.Core.Collections, dxEMF.Types;

type
  //<IFilmExpression
  IFilmExpression = interface(IdxEntityInfo)
  ['{3CBFEC0D-63E0-4E9B-B618-7D358F42326E}']
    function ID: TdxLinqExpression;
    function Color: TdxLinqExpression;
    function Plotoutline: TdxLinqExpression;
    function Runtime: TdxLinqExpression;
    function Tagline: TdxLinqExpression;
    function Title: TdxLinqExpression;
    function Views: TdxLinqExpression;
    function Website: TdxLinqExpression;
    function Year: TdxLinqExpression;
  end;
  //>IFilmExpression

  //<TFilm
  [Entity]
  [Automapping]
  TFilm = class
  strict private
    FID: Integer;
    [Nullable]
    FColor: Boolean;
    [Size(300), Nullable]
    FPlotoutline: string;
    [Nullable]
    FRuntime: Integer;
    [Size(300), Nullable]
    FTagline: string;
    [Nullable]
    FTitle: string;
    [Size(120), Nullable]
    FWebsite: string;
    [Nullable]
    FViews: Integer;
    [Nullable]
    FYear: Integer;
  public
    property ID: Integer read FID;
    property Color: Boolean read FColor write FColor;
    property Plotoutline: string read FPlotoutline write FPlotoutline;
    property Runtime: Integer read FRuntime write FRuntime;
    property Tagline: string read FTagline write FTagline;
    property Title: string read FTitle write FTitle;
    property Views: Integer read FViews write FViews;
    property Website: string read FWebsite write FWebsite;
    property Year: Integer read FYear write FYear;
  end;
  //>TFilm

  //<IMailExpression
  IMailExpression = interface(IdxEntityInfo)
  ['{325B673D-2BFA-452E-BE61-08BF851D513E}']
    function ID: TdxLinqExpression;
    function From: TdxLinqExpression;
    function HasAttachment: TdxLinqExpression;
    function Priority: TdxLinqExpression;
    function Sent: TdxLinqExpression;
    function Size: TdxLinqExpression;
    function Subject: TdxLinqExpression;
  end;
  //>IMailExpression

  //<TMail
  [Entity]
  [Automapping]
  TMail = class
  strict private
    FID: Integer;
    [Column('From')]
    FFrom: string;
    FHasAttachment: Boolean;
    FSent: TDateTime;
    FSize: Int64;
    [Column('Subject'), Size(255)]
    FSubject: string;
    FPriority: Integer;
  public
    property ID: Integer read FID write FID;
    property From: string read FFrom write FFrom;
    property HasAttachment: Boolean read FHasAttachment write FHasAttachment;
    property Priority: Integer read FPriority write FPriority;
    property Sent: TDateTime read FSent write FSent;
    property Size: Int64 read FSize write FSize;
    property Subject: string read FSubject write FSubject;
  end;
  //>TMail

  TTeam = class;

  //<IMemberExpression
  IMemberExpression = interface(IdxEntityInfo)
  ['{BF4B4B6B-C65B-4167-9404-9C72711342B2}']
    function ID: TdxLinqExpression;
    function EMail: TdxLinqExpression;
    function Name: TdxLinqExpression;
    function Team: TdxLinqExpression;
  end;
  //>IMemberExpression

  //<TMember
  [Entity]
  [Table('Member')]
  TMember = class
  private
    [Column('ID'), Generator(TdxGeneratorType.Identity), Indexed, Key]
    FID: Integer;
    [Column('EMail')]
    FEMail: string;
    [Column('Name'), Size(50)]
    FName: string;
    [Association]
    FTeam: TTeam;
  public
    property ID: Integer read FID write FID;
    property EMail: string read FEMail write FEMail;
    property Name: string read FName write FName;
    property Team: TTeam read FTeam write FTeam;
  end;
  //>TMember

  //<IdxMemberCollectionExpression
  IdxMemberCollectionExpression = interface(IdxLinqCollectionExpression<IMemberExpression>)
  ['{86D9E243-B9FD-4C5F-8AC3-DAEF57890734}']
  end;
  //>IdxMemberCollectionExpression

  //<ITeamExpression
  ITeamExpression = interface(IdxEntityInfo)
  ['{B735779F-2349-4F5F-ABA1-04B8089BA1F8}']
    function ID: TdxLinqExpression;
    function Name: TdxLinqExpression;
    function Members: IdxMemberCollectionExpression;
  end;
  //>ITeamExpression

  //<TTeam
  [Entity]
  [Table('Team')]
  TTeam = class
  private
    [Column('ID'), Generator(TdxGeneratorType.Identity), Indexed, Key]
    FID: Integer;
    [Column('Name'), Size(50)]
    FName: string;
    [Association, Aggregated]
    FMembers: IdxEMFCollection<TMember>;
    [Column('Products')]
    FProducts: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    property ID: Integer read FID write FID;
    property Name: string read FName write FName;
    property Members: IdxEMFCollection<TMember> read FMembers write FMembers;
    property Products: TStringList read FProducts write FProducts;
  end;
  //>TTeam

  //<IDemoContext
  IDemoContext = interface(IdxDataContext)
  ['{4DB481A1-7A3E-4761-8DB7-569FF7D9593A}']
    function Film: IFilmExpression;
    function Mail: IMailExpression;
    function Member: IMemberExpression;
    function Team: ITeamExpression;
  end;
  //>IDemoContext

implementation

//<ITeamExpression
//<TTeam

constructor TTeam.Create;
begin
  inherited Create;
  FProducts := TStringList.Create;
  FMembers := TdxEMFCollections.Create<TMember>(Self, 'Members');
end;

destructor TTeam.Destroy;
begin
  FreeAndNil(FProducts);
  inherited Destroy;
end;
//>TTeam
//>ITeamExpression

initialization
  TdxLinqExpressionFactory.Register<TFilm, IFilmExpression>;
  TdxLinqExpressionFactory.Register<TMail, IMailExpression>;
  TdxLinqExpressionFactory.Register<TMember, IMemberExpression>;
  TdxLinqExpressionFactory.Register<TTeam, ITeamExpression>;

end.
