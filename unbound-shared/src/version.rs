use std::{fmt::Display, num::NonZeroU64, ops::RangeInclusive};

/// A version as defined by [Semantic Versioning](https://semver.org/).
///
/// This implementation is stripped down slightly and also a bit more strict to keep it simple and
/// easier to work with. Notably, pre-release and build metadata are not supported; only major,
/// minor and patch number are.
///
/// Major versions before `1.0.0` have stricter requirements than semver: The minor version must be
/// treated as a major version and patch version must be treated as a minor version. This means,
/// before the `1.0.0` release there is no semantic equivalent to the patch version number.
///
/// The [`Default`] version is `0.1.0`. This is also the lowest possible version. Versions such as
/// `0.0.1` or `0.0.0` are not possible.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Version {
    Dev(DevVersion),
    Release(ReleaseVersion),
}

impl Version {
    /// The lowest possible [`Version`], `0.1.0`.
    ///
    /// This is also the [`Default`].
    pub const MIN: Self = Self::Dev(DevVersion::MIN);

    /// The highest possible [`Version`] made up of [`u64::MAX`] for each component.
    pub const MAX: Self = Self::Release(ReleaseVersion::MAX);

    /// Constructs a new [`Version`] from the given major, minor and patch number.
    ///
    /// # Panics
    ///
    /// Panics if both `major` and `minor` are `0`, as the minimum version is `0.1.0`.
    pub fn new(major: u64, minor: u64, patch: u64) -> Self {
        if let Some(major) = NonZeroU64::new(major) {
            Self::Release(ReleaseVersion::new(major, minor, patch))
        } else {
            let minor = NonZeroU64::new(minor).expect("version must be at least 0.1.0");
            Self::Dev(DevVersion::new(minor, patch))
        }
    }

    /// Returns `true` if the version is not yet released.
    pub fn is_dev(self) -> bool {
        matches!(self, Self::Dev(..))
    }

    /// Returns the [`DevVersion`] if the version is not yet released.
    pub fn as_dev(self) -> Option<DevVersion> {
        if let Self::Dev(dev) = self {
            Some(dev)
        } else {
            None
        }
    }

    /// Returns `true` if the version is released.
    pub fn is_release(self) -> bool {
        matches!(self, Self::Release(..))
    }

    /// Returns the [`ReleaseVersion`] if the version is released.
    pub fn as_release(self) -> Option<ReleaseVersion> {
        if let Self::Release(release) = self {
            Some(release)
        } else {
            None
        }
    }

    /// The major version number.
    pub fn major(self) -> u64 {
        if let Version::Release(ReleaseVersion { major, .. }) = self {
            major.get()
        } else {
            0
        }
    }

    /// The minor version number.
    pub fn minor(self) -> u64 {
        match self {
            Version::Release(ReleaseVersion { minor, .. }) => minor,
            Version::Dev(DevVersion { minor, .. }) => minor.get(),
        }
    }

    /// The patch version number.
    pub fn patch(self) -> u64 {
        match self {
            Version::Release(ReleaseVersion { patch, .. })
            | Version::Dev(DevVersion { patch, .. }) => patch,
        }
    }

    /// Returns an array containing major, minor and patch version numbers in that order.
    pub fn as_array(self) -> [u64; 3] {
        [self.major(), self.minor(), self.patch()]
    }

    /// Bumps the version due to the given change.
    ///
    /// # Panics
    ///
    /// Panics if the bump causes a version number to overflow.
    pub fn bump(self, change: Change) -> Self {
        match self {
            Version::Dev(dev) => Version::Dev(dev.bump(change)),
            Version::Release(release) => Version::Release(release.bump(change)),
        }
    }

    /// Whether this version is compatible with the given requirement.
    pub fn meets(self, requirement: VersionReq) -> bool {
        requirement.version_range().contains(&self)
    }
}

impl Default for Version {
    fn default() -> Self {
        Self::MIN
    }
}

impl Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Version::Dev(dev) => dev.fmt(f),
            Version::Release(release) => release.fmt(f),
        }
    }
}

/// A version that is still in development with major version `0`.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct DevVersion {
    /// Treated as a [major](https://semver.org/#spec-item-8) version number.
    pub minor: NonZeroU64,
    /// Treated as a [minor](https://semver.org/#spec-item-7) version number.
    pub patch: u64,
}

impl DevVersion {
    /// The lowest possible [`DevVersion`], `0.1.0`.
    ///
    /// This is also the [`Default`].
    pub const MIN: Self = Self {
        minor: NonZeroU64::MIN,
        patch: 0,
    };

    /// The highest possible [`DevVersion`] with [`u64::MAX`] for both minor and patch number.
    pub const MAX: Self = Self {
        minor: NonZeroU64::MAX,
        patch: u64::MAX,
    };

    /// Creates a new [`DevVersion`] from the given minor and patch number.
    pub fn new(minor: NonZeroU64, patch: u64) -> Self {
        Self { minor, patch }
    }

    /// Bumps the version due to the given change.
    ///
    /// - [`Change::Breaking`] bumps minor
    /// - [`Change::BackwardsCompatible`] and [`Change::Fix`] bump patch
    ///
    /// # Panics
    ///
    /// Panics if the bump causes a version number to overflow.
    pub fn bump(self, reason: Change) -> Self {
        match reason {
            Change::Breaking => Self {
                minor: self.minor.checked_add(1).expect("minor version overflow"),
                patch: 0,
            },
            Change::BackwardsCompatible | Change::Fix => Self {
                patch: self.patch.checked_add(1).expect("patch version overflow"),
                ..self
            },
        }
    }

    /// Whether this version is compatible with the given requirement.
    pub fn meets(self, requirement: DevVersionReq) -> bool {
        requirement.version_range().contains(&self)
    }
}

impl Default for DevVersion {
    fn default() -> Self {
        Self::MIN
    }
}

impl Display for DevVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "0.{}.{}", self.minor, self.patch)
    }
}

/// A fully released version with major version being at least `1`.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ReleaseVersion {
    /// The [major](https://semver.org/#spec-item-8) version number.
    pub major: NonZeroU64,
    /// The [minor](https://semver.org/#spec-item-7) version number.
    pub minor: u64,
    /// The [patch](https://semver.org/#spec-item-6) version number.
    pub patch: u64,
}

impl ReleaseVersion {
    /// The lowest possible [`ReleaseVersion`], `1.0.0`.
    ///
    /// This is also the [`Default`].
    pub const MIN: Self = Self {
        major: NonZeroU64::MIN,
        minor: 0,
        patch: 0,
    };

    /// The highest possible [`ReleaseVersion`] made up of [`u64::MAX`] for each component.
    pub const MAX: Self = Self {
        major: NonZeroU64::MAX,
        minor: u64::MAX,
        patch: u64::MAX,
    };

    /// Creates a new [`ReleaseVersion`] from the given minor and patch number.
    pub fn new(major: NonZeroU64, minor: u64, patch: u64) -> Self {
        Self {
            major,
            minor,
            patch,
        }
    }

    /// Bumps the version due to the given change.
    ///
    /// - [`Change::Breaking`] bumps major
    /// - [`Change::BackwardsCompatible`] bumps minor
    /// - [`Change::Fix`] bumps patch
    ///
    /// # Panics
    ///
    /// Panics if the bump causes a version number to overflow.
    pub fn bump(self, change: Change) -> Self {
        match change {
            Change::Breaking => Self {
                major: self.major.checked_add(1).expect("major version overflow"),
                minor: 0,
                patch: 0,
            },
            Change::BackwardsCompatible => Self {
                minor: self.minor.checked_add(1).expect("minor version overflow"),
                patch: 0,
                ..self
            },
            Change::Fix => Self {
                patch: self.patch.checked_add(1).expect("patch version overflow"),
                ..self
            },
        }
    }

    /// Whether this version is compatible with the given requirement.
    pub fn meets(self, requirement: ReleaseVersionReq) -> bool {
        requirement.version_range().contains(&self)
    }
}

impl Default for ReleaseVersion {
    fn default() -> Self {
        Self::MIN
    }
}

impl Display for ReleaseVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

/// A requirement for a specific range of [`Version`]s.
///
/// A requirement can be generated [`From`] a (minimum) [`Version`], which will discard the patch
/// number for released versions. It can be converted back into a [Version] using
/// [`Self::min_version`], [`Self::max_version`] or [`Self::version_range`].
///
/// Will always be [`Display`]ed in the format `>=0.1.0` to keep it concise. It could also be
/// displayed as a range, which would make the meaning more clear, but it's also a bit more verbose.
///
/// Intentionally does _not_ implement [`Default`].
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum VersionReq {
    Dev(DevVersionReq),
    Release(ReleaseVersionReq),
}

impl VersionReq {
    /// Creates a new dev version requirement with the given minor and patch version.
    pub fn dev(minor_exact: NonZeroU64, patch_min: u64) -> Self {
        Self::Dev(DevVersionReq::new(minor_exact, patch_min))
    }

    /// Creates a new release version requirement with the given major and minor version.
    pub fn release(major_exact: NonZeroU64, minor_min: u64) -> Self {
        Self::Release(ReleaseVersionReq::new(major_exact, minor_min))
    }

    /// The minimum [`Version`] that meets this requirement.
    pub fn min_version(self) -> Version {
        match self {
            Self::Dev(dev) => Version::Dev(dev.min_version()),
            Self::Release(release) => Version::Release(release.min_version()),
        }
    }

    /// The maximum [`Version`] that meets this requirement.
    pub fn max_version(self) -> Version {
        match self {
            Self::Dev(dev) => Version::Dev(dev.max_version()),
            Self::Release(release) => Version::Release(release.max_version()),
        }
    }

    /// All range containing all [`Version`]s that meet this requirement.
    pub fn version_range(self) -> RangeInclusive<Version> {
        self.min_version()..=self.max_version()
    }
}

impl Display for VersionReq {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ">={}", self.min_version())
    }
}

impl From<Version> for VersionReq {
    fn from(value: Version) -> Self {
        match value {
            Version::Dev(DevVersion { minor, patch }) => Self::Dev(DevVersionReq {
                minor_exact: minor,
                patch_min: patch,
            }),
            Version::Release(ReleaseVersion { major, minor, .. }) => {
                Self::Release(ReleaseVersionReq {
                    major_exact: major,
                    minor_min: minor,
                })
            }
        }
    }
}

/// A version requirement for a dev version with major version `0`.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct DevVersionReq {
    /// The minor version that has to match exactly due to being pre `1.0.0`.
    pub minor_exact: NonZeroU64,
    /// The patch version that has to match "at least" due to being pre `1.0.0`.
    pub patch_min: u64,
}

impl DevVersionReq {
    /// Creates a new dev version requirement with the given minor and patch version.
    pub fn new(minor_exact: NonZeroU64, patch_min: u64) -> Self {
        Self {
            minor_exact,
            patch_min,
        }
    }

    /// The minimum [`DevVersion`] that meets this requirement.
    pub fn min_version(self) -> DevVersion {
        DevVersion {
            minor: self.minor_exact,
            patch: self.patch_min,
        }
    }

    /// The maximum [`DevVersion`] that meets this requirement.
    pub fn max_version(self) -> DevVersion {
        DevVersion {
            minor: self.minor_exact,
            patch: u64::MAX,
        }
    }

    /// All range containing all [`DevVersion`]s that meet this requirement.
    pub fn version_range(self) -> RangeInclusive<DevVersion> {
        self.min_version()..=self.max_version()
    }
}

/// A version requirement for a released version with major version at least `1`.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ReleaseVersionReq {
    /// The major version that has to match exactly.
    pub major_exact: NonZeroU64,
    /// The minimum minor version that will match.
    pub minor_min: u64,
}

impl ReleaseVersionReq {
    /// Creates a new release version requirement with the given major and minor version.
    pub fn new(major_exact: NonZeroU64, minor_min: u64) -> Self {
        Self {
            major_exact,
            minor_min,
        }
    }

    /// The minimum [`ReleaseVersion`] that meets this requirement.
    pub fn min_version(self) -> ReleaseVersion {
        ReleaseVersion {
            major: self.major_exact,
            minor: self.minor_min,
            patch: 0,
        }
    }

    /// The maximum [`ReleaseVersion`] that meets this requirement.
    pub fn max_version(self) -> ReleaseVersion {
        ReleaseVersion {
            major: self.major_exact,
            minor: u64::MAX,
            patch: u64::MAX,
        }
    }

    /// All range containing all [`ReleaseVersion`]s that meet this requirement.
    pub fn version_range(self) -> RangeInclusive<ReleaseVersion> {
        self.min_version()..=self.max_version()
    }
}

/// Indicates the severity of a change between two [`Version`]s.
///
/// Ordered by severity; [`Change::Fix`] has the least impact and thus comes first.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Change {
    /// A bug fix that doesn't touch public API.
    ///
    /// Apart from the change in behavior due to the bug fix, other behavior must remain the same.
    Fix,
    /// A backwards compatible change like adding new functionality without changing existing one.
    BackwardsCompatible,
    /// A breaking change like changing existing functionality or removing it.
    ///
    /// Requires a major version bump.
    Breaking,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_version() {
        let version = Version::new(0, 1, 2);
        assert_eq!(
            version,
            Version::Dev(DevVersion {
                minor: NonZeroU64::new(1).unwrap(),
                patch: 2,
            })
        );

        let version = Version::new(1, 2, 3);
        assert_eq!(
            version,
            Version::Release(ReleaseVersion {
                major: NonZeroU64::new(1).unwrap(),
                minor: 2,
                patch: 3,
            })
        );
    }

    #[test]
    #[should_panic = "version must be at least 0.1.0"]
    fn new_version_panics() {
        Version::new(0, 0, 1);
    }

    #[test]
    fn is_dev() {
        let version = Version::new(0, 1, 2);
        assert!(version.is_dev());
        assert!(!version.is_release());
    }

    #[test]
    fn is_release() {
        let version = Version::new(1, 2, 3);
        assert!(version.is_release());
        assert!(!version.is_dev());
    }

    #[test]
    fn as_dev() {
        let version = Version::new(0, 1, 2);
        assert_eq!(
            version.as_dev(),
            Some(DevVersion {
                minor: NonZeroU64::MIN,
                patch: 2,
            })
        );

        let version = Version::new(1, 2, 3);
        assert_eq!(version.as_dev(), None);
    }

    #[test]
    fn as_release() {
        let version = Version::new(1, 2, 3);
        assert_eq!(
            version.as_release(),
            Some(ReleaseVersion {
                major: NonZeroU64::MIN,
                minor: 2,
                patch: 3,
            })
        );

        let version = Version::new(0, 1, 2);
        assert_eq!(version.as_release(), None);
    }

    #[test]
    fn major_minor_patch() {
        let version = Version::new(0, 1, 2);
        assert_eq!(version.major(), 0);
        assert_eq!(version.minor(), 1);
        assert_eq!(version.patch(), 2);

        let version = Version::new(1, 2, 3);
        assert_eq!(version.major(), 1);
        assert_eq!(version.minor(), 2);
        assert_eq!(version.patch(), 3);
    }

    #[test]
    fn as_array() {
        let version = Version::new(1, 2, 3);
        assert_eq!(version.as_array(), [1, 2, 3]);

        let version = Version::new(0, 1, 2);
        assert_eq!(version.as_array(), [0, 1, 2]);
    }

    #[test]
    fn bump_dev_version() {
        let version = Version::new(0, 1, 2);
        assert_eq!(version.bump(Change::Fix), Version::new(0, 1, 3));
        assert_eq!(
            version.bump(Change::BackwardsCompatible),
            Version::new(0, 1, 3)
        );
        assert_eq!(version.bump(Change::Breaking), Version::new(0, 2, 0));
    }

    #[test]
    fn bump_release_version() {
        let version = Version::new(1, 2, 3);
        assert_eq!(version.bump(Change::Fix), Version::new(1, 2, 4));
        assert_eq!(
            version.bump(Change::BackwardsCompatible),
            Version::new(1, 3, 0)
        );
        assert_eq!(version.bump(Change::Breaking), Version::new(2, 0, 0));
    }

    #[test]
    #[should_panic = "patch version overflow"]
    fn bump_dev_version_patch_overflow() {
        Version::new(0, 1, u64::MAX).bump(Change::BackwardsCompatible);
    }

    #[test]
    #[should_panic = "minor version overflow"]
    fn bump_dev_version_minor_overflow() {
        Version::new(0, u64::MAX, 0).bump(Change::Breaking);
    }

    #[test]
    #[should_panic = "patch version overflow"]
    fn bump_release_version_patch_overflow() {
        Version::new(1, 0, u64::MAX).bump(Change::Fix);
    }

    #[test]
    #[should_panic = "minor version overflow"]
    fn bump_release_version_minor_overflow() {
        Version::new(1, u64::MAX, 0).bump(Change::BackwardsCompatible);
    }

    #[test]
    #[should_panic = "major version overflow"]
    fn bump_release_version_major_overflow() {
        Version::new(u64::MAX, 0, 0).bump(Change::Breaking);
    }

    #[test]
    fn display_version() {
        let version = Version::new(0, 1, 2);
        assert_eq!(version.to_string(), "0.1.2");

        let version = Version::new(1, 2, 3);
        assert_eq!(version.to_string(), "1.2.3");
    }

    #[test]
    fn meets_dev_version_req() {
        let require_0_2_1 = VersionReq::dev(NonZeroU64::new(2).unwrap(), 1);

        assert!(!Version::new(0, 2, 0).meets(require_0_2_1));
        assert!(Version::new(0, 2, 1).meets(require_0_2_1));
        assert!(Version::new(0, 2, u64::MAX).meets(require_0_2_1));
        assert!(!Version::new(0, 3, 0).meets(require_0_2_1));
    }

    #[test]
    fn meets_release_version_req() {
        let require_1_2_0 = VersionReq::release(NonZeroU64::MIN, 2);

        assert!(!Version::new(1, 1, u64::MAX).meets(require_1_2_0));
        assert!(Version::new(1, 2, 0).meets(require_1_2_0));
        assert!(Version::new(1, u64::MAX, u64::MAX).meets(require_1_2_0));
        assert!(!Version::new(2, 0, 0).meets(require_1_2_0));
    }

    #[test]
    fn version_order() {
        let versions = [
            Version::MIN, // 0.1.0
            Version::new(0, 1, 1),
            Version::new(0, 2, 0),
            Version::new(0, 2, 1),
            Version::new(1, 0, 0),
            Version::new(1, 0, 1),
            Version::new(1, 1, 0),
            Version::new(1, 1, 1),
            Version::MAX, // MAX.MAX.MAX
        ];
        assert!(versions.windows(2).all(|w| w[0] < w[1]));
    }
}
