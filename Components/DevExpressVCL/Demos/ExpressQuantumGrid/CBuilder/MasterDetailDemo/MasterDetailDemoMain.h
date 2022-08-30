//---------------------------------------------------------------------------

#ifndef MasterDetailDemoMainH
#define MasterDetailDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDBData.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxGrid.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxGridCardView.hpp"
#include "cxGridDBCardView.hpp"
#include "cxLookAndFeels.hpp"
#include "BaseForm.h"
#include "cxBlobEdit.hpp"
#include "cxDataStorage.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxGridCustomLayoutView.hpp"
#include "cxImage.hpp"
#include "cxLookAndFeelPainters.hpp"
//---------------------------------------------------------------------------
class TMasterDetailDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
  TcxGrid *Grid;
  TcxGridDBTableView *tvFilms;
  TcxGridDBColumn *tvFilmsID;
  TcxGridDBColumn *tvFilmsCAPTION;
  TcxGridDBColumn *tvFilmsYEAR;
  TcxGridDBColumn *tvFilmsTAGLINE;
  TcxGridDBColumn *tvFilmsPLOTOUTLINE;
  TcxGridDBColumn *tvFilmsRUNTIME;
  TcxGridDBColumn *tvFilmsCOLOR;
  TcxGridDBColumn *tvFilmsPHOTO;
  TcxGridDBColumn *tvFilmsICON;
  TcxGridDBColumn *tvFilmsWEBSITE;
  TcxGridDBCardView *cvPeople;
  TcxGridDBCardViewRow *cvPeopleName;
  TcxGridDBCardViewRow *cvPeoplePersonLineID;
  TcxGridDBCardViewRow *cvPeopleFIRSTNAME;
  TcxGridDBCardViewRow *cvPeopleSECONDNAME;
  TcxGridDBCardViewRow *cvPeopleNICKNAME;
  TcxGridDBCardViewRow *cvPeopleBIRTHNAME;
  TcxGridDBCardViewRow *cvPeopleDATEOFBIRTH;
  TcxGridDBCardViewRow *cvPeopleLOCATIONOFBIRTH;
  TcxGridDBCardViewRow *cvPeopleBIOGRAPHY;
  TcxGridDBCardViewRow *cvPeopleHOMEPAGE;
  TcxGridDBCardViewRow *cvPeopleID;
  TcxGridDBCardViewRow *cvPeopleFilmID;
  TcxGridDBCardViewRow *cvPeopleBIRTHCOUNTRY;
  TcxGridDBCardViewRow *cvPeopleGender;
  TcxGridDBTableView *tvCompanies;
  TcxGridDBColumn *tvCompaniesName;
  TcxGridDBColumn *tvCompaniesType;
  TcxGridDBColumn *tvCompaniesCountry;
  TcxGridDBColumn *tvCompaniesWebSite;
  TcxGridDBColumn *tvCompaniesID;
  TcxGridDBColumn *tvCompaniesFILMID;
  TcxGridDBCardView *cvPhotos;
  TcxGridDBCardViewRow *cvPhotosID;
  TcxGridDBCardViewRow *cvPhotosFILMID;
  TcxGridDBCardViewRow *cvPhotosSCREEN;
  TcxGridDBCardViewRow *cvPhotosICON;
  TcxGridLevel *lvFilms;
  TcxGridLevel *lvPeople;
  TcxGridLevel *lvCompanies;
  TcxGridLevel *lvPhotos;
  TMenuItem *miOptions;
  TMenuItem *miTabsPosition;
  TMenuItem *miLeftTabsPosition;
  TMenuItem *miTopTabsPosition;
  TMenuItem *miDetailViewsSynchronization;
  TMenuItem *miSeparator1;
  TMenuItem *miShowPreviewData;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall miShowPreviewDataClick(TObject *Sender);
  void __fastcall miTabsPositionClick(TObject *Sender);
  void __fastcall miDetailViewsSynchronizationClick(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
private:  // User declarations
public:   // User declarations
  __fastcall TMasterDetailDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMasterDetailDemoMainForm *MasterDetailDemoMainForm;
//---------------------------------------------------------------------------
#endif
