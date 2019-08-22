create table Address (
  AddressID text not null
  , AddressLine1 text not null
  , AddressLine2 text not null
  , City text not null
  , StateProvinceID text not null
  , PostalCode text not null
  , SpatialLocation text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table AddressType (
  AddressTypeID text not null
  , Name text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table AWBuildVersion (
  SystemInformationID text not null
  , DatabaseVersion text not null
  , VersionDate text not null
  , ModifiedDate text not null
);

create table BillOfMaterials (
  BillOfMaterialsID text not null
  , ProductAssemblyID text not null
  , ComponentID text not null
  , StartDate text not null
  , EndDate text not null
  , UnitMeasureCode text not null
  , BOMLevel text not null
  , PerAssemblyQty text not null
  , ModifiedDate text not null
);

create table BusinessEntity (
  BusinessEntityID text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table BusinessEntityAddress (
  BusinessEntityID text not null
  , AddressID text not null
  , AddressTypeID text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table BusinessEntityContact (
  BusinessEntityID text not null
  , PersonID text not null
  , ContactTypeID text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table ContactType (
  ContactTypeID text not null
  , Name text not null
  , ModifiedDate text not null
);

create table CountryRegion (
  CountryRegionCode text not null
  , Name text not null
  , ModifiedDate text not null
);

create table CountryRegionCurrency (
  CountryRegionCode text not null
  , CurrencyCode text not null
  , ModifiedDate text not null
);

create table CreditCard (
  CreditCardID text not null
  , CardType text not null
  , CardNumber text not null
  , ExpMonth text not null
  , ExpYear text not null
  , ModifiedDate text not null
);

create table Culture (
  CultureID text not null
  , Name text not null
  , ModifiedDate text not null
);

create table Currency (
  CurrencyCode text not null
  , Name text not null
  , ModifiedDate text not null
);

create table CurrencyRate (
  CurrencyRateID text not null
  , CurrencyRateDate text not null
  , FromCurrencyCode text not null
  , ToCurrencyCode text not null
  , AverageRate text not null
  , EndOfDayRate text not null
  , ModifiedDate text not null
);

create table Customer (
  CustomerID text not null
  , PersonID text not null
  , StoreID text not null
  , TerritoryID text not null
  , AccountNumber text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table DatabaseLog (
  DatabaseLogID text not null
  , PostTime text not null
  , DatabaseUser text not null
  , Event text not null
  , Schema text not null
  , Object text not null
  , TSQL text not null
  , XmlEvent text not null
);

create table Department (
  DepartmentID text not null
  , Name text not null
  , GroupName text not null
  , ModifiedDate text not null
);

create table Document (
  DocumentNode text not null
  , DocumentLevel text not null
  , Title text not null
  , Owner text not null
  , FolderFlag text not null
  , FileName text not null
  , FileExtension text not null
  , Revision text not null
  , ChangeNumber text not null
  , Status text not null
  , DocumentSummary text not null
  , Document text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table EmailAddress (
  BusinessEntityID text not null
  , EmailAddressID text not null
  , EmailAddress text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table Employee (
  BusinessEntityID text not null
  , NationalIDNumber text not null
  , LoginID text not null
  , OrganizationNode text not null
  , OrganizationLevel text not null
  , JobTitle text not null
  , BirthDate text not null
  , MaritalStatus text not null
  , Gender text not null
  , HireDate text not null
  , SalariedFlag text not null
  , VacationHours text not null
  , SickLeaveHours text not null
  , CurrentFlag text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table EmployeeDepartmentHistory (
  BusinessEntityID text not null
  , DepartmentID text not null
  , ShiftID text not null
  , StartDate text not null
  , EndDate text not null
  , ModifiedDate text not null
);

create table EmployeePayHistory (
  BusinessEntityID text not null
  , RateChangeDate text not null
  , Rate text not null
  , PayFrequency text not null
  , ModifiedDate text not null
);

create table ErrorLog (
  ErrorLogID text not null
  , ErrorTime text not null
  , UserName text not null
  , ErrorNumber text not null
  , ErrorSeverity text not null
  , ErrorState text not null
  , ErrorProcedure text not null
  , ErrorLine text not null
  , ErrorMessage text not null
);

create table Illustration (
  IllustrationID text not null
  , Diagram text not null
  , ModifiedDate text not null
);

create table JobCandidate (
  JobCandidateID text not null
  , BusinessEntityID text not null
  , Resume text not null
  , ModifiedDate text not null
);

create table Location (
  LocationID text not null
  , Name text not null
  , CostRate text not null
  , Availability text not null
  , ModifiedDate text not null
);

create table Password (
  BusinessEntityID text not null
  , PasswordHash text not null
  , PasswordSalt text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table Person (
  BusinessEntityID text not null
  , PersonType text not null
  , NameStyle text not null
  , Title text not null
  , FirstName text not null
  , MiddleName text not null
  , LastName text not null
  , Suffix text not null
  , EmailPromotion text not null
  , AdditionalContactInfo text not null
  , Demographics text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table PersonCreditCard (
  BusinessEntityID text not null
  , CreditCardID text not null
  , ModifiedDate text not null
);

create table PersonPhone (
  BusinessEntityID text not null
  , PhoneNumber text not null
  , PhoneNumberTypeID text not null
  , ModifiedDate text not null
);

create table PhoneNumberType (
  PhoneNumberTypeID text not null
  , Name text not null
  , ModifiedDate text not null
);

create table Product (
  ProductID text not null
  , Name text not null
  , ProductNumber text not null
  , MakeFlag text not null
  , FinishedGoodsFlag text not null
  , Color text not null
  , SafetyStockLevel text not null
  , ReorderPoint text not null
  , StandardCost text not null
  , ListPrice text not null
  , Size text not null
  , SizeUnitMeasureCode text not null
  , WeightUnitMeasureCode text not null
  , Weight text not null
  , DaysToManufacture text not null
  , ProductLine text not null
  , Class text not null
  , Style text not null
  , ProductSubcategoryID text not null
  , ProductModelID text not null
  , SellStartDate text not null
  , SellEndDate text not null
  , DiscontinuedDate text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table ProductCategory (
  ProductCategoryID text not null
  , Name text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table ProductCostHistory (
  ProductID text not null
  , StartDate text not null
  , EndDate text not null
  , StandardCost text not null
  , ModifiedDate text not null
);

create table ProductDescription (
  ProductDescriptionID text not null
  , Description text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table ProductDocument (
  ProductID text not null
  , DocumentNode text not null
  , ModifiedDate text not null
);

create table ProductInventory (
  ProductID text not null
  , LocationID text not null
  , Shelf text not null
  , Bin text not null
  , Quantity text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table ProductListPriceHistory (
  ProductID text not null
  , StartDate text not null
  , EndDate text not null
  , ListPrice text not null
  , ModifiedDate text not null
);

create table ProductModel (
  ProductModelID text not null
  , Name text not null
  , CatalogDescription text not null
  , Instructions text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table ProductModelIllustration (
  ProductModelID text not null
  , IllustrationID text not null
  , ModifiedDate text not null
);

create table ProductModelProductDescriptionCulture (
  ProductModelID text not null
  , ProductDescriptionID text not null
  , CultureID text not null
  , ModifiedDate text not null
);

create table ProductPhoto (
  ProductPhotoID text not null
  , ThumbNailPhoto text not null
  , ThumbnailPhotoFileName text not null
  , LargePhoto text not null
  , LargePhotoFileName text not null
  , ModifiedDate text not null
);

create table ProductProductPhoto (
  ProductID text not null
  , ProductPhotoID text not null
  , [Primary] text not null
  , ModifiedDate text not null
);

create table ProductReview (
  ProductReviewID text not null
  , ProductID text not null
  , ReviewerName text not null
  , ReviewDate text not null
  , EmailAddress text not null
  , Rating text not null
  , Comments text not null
  , ModifiedDate text not null
);

create table ProductSubcategory (
  ProductSubcategoryID text not null
  , ProductCategoryID text not null
  , Name text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table ProductVendor (
  ProductID text not null
  , BusinessEntityID text not null
  , AverageLeadTime text not null
  , StandardPrice text not null
  , LastReceiptCost text not null
  , LastReceiptDate text not null
  , MinOrderQty text not null
  , MaxOrderQty text not null
  , OnOrderQty text not null
  , UnitMeasureCode text not null
  , ModifiedDate text not null
);

create table PurchaseOrderDetail (
  PurchaseOrderID text not null
  , PurchaseOrderDetailID text not null
  , DueDate text not null
  , OrderQty text not null
  , ProductID text not null
  , UnitPrice text not null
  , LineTotal text not null
  , ReceivedQty text not null
  , RejectedQty text not null
  , StockedQty text not null
  , ModifiedDate text not null
);

create table PurchaseOrderHeader (
  PurchaseOrderID text not null
  , RevisionNumber text not null
  , Status text not null
  , EmployeeID text not null
  , VendorID text not null
  , ShipMethodID text not null
  , OrderDate text not null
  , ShipDate text not null
  , SubTotal text not null
  , TaxAmt text not null
  , Freight text not null
  , TotalDue text not null
  , ModifiedDate text not null
);

create table SalesOrderDetail (
  SalesOrderID text not null
  , SalesOrderDetailID text not null
  , CarrierTrackingNumber text not null
  , OrderQty text not null
  , ProductID text not null
  , SpecialOfferID text not null
  , UnitPrice text not null
  , UnitPriceDiscount text not null
  , LineTotal text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table SalesOrderHeader (
  SalesOrderID text not null
  , RevisionNumber text not null
  , OrderDate text not null
  , DueDate text not null
  , ShipDate text not null
  , Status text not null
  , OnlineOrderFlag text not null
  , SalesOrderNumber text not null
  , PurchaseOrderNumber text not null
  , AccountNumber text not null
  , CustomerID text not null
  , SalesPersonID text not null
  , TerritoryID text not null
  , BillToAddressID text not null
  , ShipToAddressID text not null
  , ShipMethodID text not null
  , CreditCardID text not null
  , CreditCardApprovalCode text not null
  , CurrencyRateID text not null
  , SubTotal text not null
  , TaxAmt text not null
  , Freight text not null
  , TotalDue text not null
  , Comment text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table SalesOrderHeaderSalesReason (
  SalesOrderID text not null
  , SalesReasonID text not null
  , ModifiedDate text not null
);

create table SalesPerson (
  BusinessEntityID text not null
  , TerritoryID text not null
  , SalesQuota text not null
  , Bonus text not null
  , CommissionPct text not null
  , SalesYTD text not null
  , SalesLastYear text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table SalesPersonQuotaHistory (
  BusinessEntityID text not null
  , QuotaDate text not null
  , SalesQuota text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table SalesReason (
  SalesReasonID text not null
  , Name text not null
  , ReasonType text not null
  , ModifiedDate text not null
);

create table SalesTaxRate (
  SalesTaxRateID text not null
  , StateProvinceID text not null
  , TaxType text not null
  , TaxRate text not null
  , Name text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table SalesTerritory (
  TerritoryID text not null
  , Name text not null
  , CountryRegionCode text not null
  , [Group] text not null
  , SalesYTD text not null
  , SalesLastYear text not null
  , CostYTD text not null
  , CostLastYear text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table SalesTerritoryHistory (
  BusinessEntityID text not null
  , TerritoryID text not null
  , StartDate text not null
  , EndDate text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table ScrapReason (
  ScrapReasonID text not null
  , Name text not null
  , ModifiedDate text not null
);

create table Shift (
  ShiftID text not null
  , Name text not null
  , StartTime text not null
  , EndTime text not null
  , ModifiedDate text not null
);

create table ShipMethod (
  ShipMethodID text not null
  , Name text not null
  , ShipBase text not null
  , ShipRate text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table ShoppingCartItem (
  ShoppingCartItemID text not null
  , ShoppingCartID text not null
  , Quantity text not null
  , ProductID text not null
  , DateCreated text not null
  , ModifiedDate text not null
);

create table SpecialOffer (
  SpecialOfferID text not null
  , Description text not null
  , DiscountPct text not null
  , Type text not null
  , Category text not null
  , StartDate text not null
  , EndDate text not null
  , MinQty text not null
  , MaxQty text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table SpecialOfferProduct (
  SpecialOfferID text not null
  , ProductID text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table StateProvince (
  StateProvinceID text not null
  , StateProvinceCode text not null
  , CountryRegionCode text not null
  , IsOnlyStateProvinceFlag text not null
  , Name text not null
  , TerritoryID text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table Store (
  BusinessEntityID text not null
  , Name text not null
  , SalesPersonID text not null
  , Demographics text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table TransactionHistory (
  TransactionID text not null
  , ProductID text not null
  , ReferenceOrderID text not null
  , ReferenceOrderLineID text not null
  , TransactionDate text not null
  , TransactionType text not null
  , Quantity text not null
  , ActualCost text not null
  , ModifiedDate text not null
);

create table TransactionHistoryArchive (
  TransactionID text not null
  , ProductID text not null
  , ReferenceOrderID text not null
  , ReferenceOrderLineID text not null
  , TransactionDate text not null
  , TransactionType text not null
  , Quantity text not null
  , ActualCost text not null
  , ModifiedDate text not null
);

create table UnitMeasure (
  UnitMeasureCode text not null
  , Name text not null
  , ModifiedDate text not null
);

create table Vendor (
  BusinessEntityID text not null
  , AccountNumber text not null
  , Name text not null
  , CreditRating text not null
  , PreferredVendorStatus text not null
  , ActiveFlag text not null
  , PurchasingWebServiceURL text not null
  , ModifiedDate text not null
);

create table WorkOrder (
  WorkOrderID text not null
  , ProductID text not null
  , OrderQty text not null
  , StockedQty text not null
  , ScrappedQty text not null
  , StartDate text not null
  , EndDate text not null
  , DueDate text not null
  , ScrapReasonID text not null
  , ModifiedDate text not null
);

create table WorkOrderRouting (
  WorkOrderID text not null
  , ProductID text not null
  , OperationSequence text not null
  , LocationID text not null
  , ScheduledStartDate text not null
  , ScheduledEndDate text not null
  , ActualStartDate text not null
  , ActualEndDate text not null
  , ActualResourceHrs text not null
  , PlannedCost text not null
  , ActualCost text not null
  , ModifiedDate text not null
);