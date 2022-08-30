//---------------------------------------------------------------------------

#ifndef MasterDetailMultiDemoMainH
#define MasterDetailMultiDemoMainH
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
#include "cxGridBandedTableView.hpp"
#include "cxGridCardView.hpp"
#include "cxGridDBBandedTableView.hpp"
#include "cxGridDBCardView.hpp"
#include "cxLookAndFeels.hpp"
#include "cxBlobEdit.hpp"
#include "cxDataStorage.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxImage.hpp"
#include "cxMemo.hpp"
#include "cxPCPaintersFactory.hpp"
#include "BaseForm.h"
#include "cxGridCustomLayoutView.hpp"
#include "cxLookAndFeelPainters.hpp"
//---------------------------------------------------------------------------
class TMasterDetailMultiDemoMainForm : public TfmBaseForm
{
__published:	// IDE-managed Components
        TcxGrid *Grid;
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
        TcxGridDBBandedTableView *bvFilms;
        TcxGridDBBandedColumn *bvFilmsCAPTION;
        TcxGridDBBandedColumn *bvFilmsPHOTO;
        TcxGridDBBandedColumn *bvFilmsYEAR;
        TcxGridDBBandedColumn *bvFilmsRUNTIME;
        TcxGridDBBandedColumn *bvFilmsPLOTOUTLINE;
        TcxGridDBBandedColumn *bvFilmsWEBSITE;
        TMenuItem *miView;
        TMenuItem *miGenreTabPosition;
        TMenuItem *miTabPositionNone;
        TMenuItem *miTabPositionLeft;
        TMenuItem *miTabPositionTop;
        TMenuItem *miTabsForEmptyDetails;
        TMenuItem *miTabStyle;
        TMenuItem *miTabCaptionAlignment;
        TMenuItem *miTabCaptionAlignmentLeft;
        TMenuItem *miTabCaptionAlignmentRight;
        TMenuItem *miTabCaptionAlignmentCenter;
        void __fastcall FormCreate(TObject *Sender);
		void __fastcall miTabPositionClick(TObject *Sender);
		void __fastcall GridActiveTabChanged(TcxCustomGrid *Sender, TcxGridLevel *ALevel);
		void __fastcall miTabStyleClick(TObject *Sender);
		void __fastcall miTabCaptionAlignmentClick(TObject *Sender);
        void __fastcall miTabsForEmptyDetailsClick(TObject *Sender);
private:	// User declarations
  void CreateLevels();
  void AddTabStyleMenuItem(TcxPCStyleID AStyleID, const AnsiString AStyleName);
  void CreateTabStyleMenu();
protected:
  void UpdateMenu();
public:		// User declarations
  __fastcall TMasterDetailMultiDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMasterDetailMultiDemoMainForm *MasterDetailMultiDemoMainForm;
//---------------------------------------------------------------------------
#endif
