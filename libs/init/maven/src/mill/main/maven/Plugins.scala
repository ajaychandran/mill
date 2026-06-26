package mill.main.maven

import mill.main.buildgen.ModuleSpec
import mill.main.buildgen.ModuleSpec.*
import mill.main.maven.PomUtil.toMvnDep
import org.apache.maven.model.{ConfigurationContainer, Model}
import org.codehaus.plexus.util.xml.Xpp3Dom

import scala.jdk.CollectionConverters.*

class Plugins(model: Model) {

  def javacOptions: Seq[Opt] = plugin("maven-compiler-plugin").flatMap(config).fold(Nil) { dom =>
    def opt(name: String, prefix: String = "-") = value(dom, name).map(Opt(prefix + name, _))
    opt("release", "--").fold(Seq(opt("source"), opt("target")).flatten)(Seq(_)) ++
      opt("encoding") ++
      child(dom, "compilerArgs").fold(Nil)(dom =>
        Opt.groups(
          values(dom, "arg").filterNot(arg => isManagedJavacOption(arg) || isErrorProneOption(arg))
        )
      )
  }

  private def isErrorProneOption(arg: String): Boolean = arg.startsWith("-Xplugin:ErrorProne")

  def withErrorProneModule(spec: ModuleSpec): Option[ModuleSpec] =
    for {
      dom <- plugin("maven-compiler-plugin").flatMap(config)
      epArg <- child(dom, "compilerArgs").flatMap(values(_, "arg").find(isErrorProneOption))
      epArgs = epArg.split("\\s+").toSeq.tail
      // https://errorprone.info/docs/flags#maven
      epOptions = epArgs.collectFirst {
        case arg if arg.head == '@' =>
          os.read(os.Path(arg.tail)).split("\\s+").toSeq
      }.getOrElse(epArgs)
      epMvnDeps = child(dom, "annotationProcessorPaths").fold(Nil)(children(_, "path"))
        .flatMap { dom =>
          for {
            organization <- value(dom, "groupId")
            name <- value(dom, "artifactId")
            version = value(dom, "version").getOrElse("")
            classifier = value(dom, "classifier")
            _type = value(dom, "type")
            excludes = children(dom, "exclusions").flatMap { dom =>
              for {
                groupId <- value(dom, "groupId")
                artifactId <- value(dom, "artifactId")
              } yield (groupId, artifactId)
            }
          } yield MvnDep(
            organization = organization,
            name = name,
            version = version,
            classifier = classifier,
            `type` = _type,
            excludes = excludes
          )
        }
    } yield spec.withErrorProneModule(errorProneMvnDeps = epMvnDeps, errorProneOptions = epOptions)

  def withCheckstyleModule(module: ModuleSpec): Option[ModuleSpec] = for {
    plugin0 <- plugin("maven-checkstyle-plugin")
    dom <- plugin0.getExecutions.asScala.find(_.getGoals.contains("check")).flatMap(config)
    propertyExpansion = value(dom, "propertyExpansion")
    checkstyleProperties = propertyExpansion.fold(Nil) { v =>
      v.split("\\s+").toSeq.collect {
        case s"$k=$v" => (k, v)
      }
    }
    checkstyleMvnDeps = plugin0.getDependencies.asScala.toSeq.map(toMvnDep)
    checkstyleOptions =
      // https://maven.apache.org/plugins/maven-checkstyle-plugin/checkstyle-mojo.html#configLocation
      // Potential values are or a classpath resource or a URL or a filesystem path.
      value(dom, "configLocation").toSeq
        // Replace presets with path to classpath resource
        .map {
          case "sun_checks.xml" => "/sun_checks.xml"
          case "google_checks.xml" => "/google_checks.xml"
          case path => path
        }
        .flatMap(Seq("-c", _))
  } yield module.withCheckstyleModule.copy(
    checkstyleProperties = Values(checkstyleProperties, appendSuper = true),
    checkstyleMvnDeps = checkstyleMvnDeps,
    checkstyleOptions = checkstyleOptions
  )

  def withPmdModule(module: ModuleSpec): Option[ModuleSpec] = for {
    plugin0 <- plugin("maven-pmd-plugin")
    dom <- config(plugin0)
    // https://docs.pmd-code.org/latest/pmd_userdocs_cli_reference.html
    // Path to a ruleset xml file. The path may reference a resource on the classpath of the application, be a local file system path, or a URL. 
    pmdRulesets = child(dom, "rulesets").toSeq.flatMap(values(_, "ruleset"))
    pmdVersion = plugin0.getDependencies.asScala.collectFirst {
      case dep if dep.getGroupId == "net.sourceforge.pmd" => dep.getVersion
    }
  } yield module.withPmdModule.copy(
    pmdOptions = if (pmdRulesets.isEmpty) Nil else Seq("-R", pmdRulesets.mkString(",")),
    pmdVersion = pmdVersion
  )

  def withSpotlessModule(module: ModuleSpec): Option[ModuleSpec] = for {
    _ <- plugin("spotless-maven-plugin", "com.diffplug.spotless")
  } yield module.withSpotlessModule

  def withRevapiModule(module: ModuleSpec): Option[ModuleSpec] = for {
    _ <- plugin("revapi-maven-plugin", "org.revapi")
  } yield module.withRevapiModule

  def withJacocoTestModule(module: ModuleSpec): Option[ModuleSpec] = for {
    _ <- plugin("jacoco-maven-plugin", "org.jacoco")
  } yield module.withJacocoTestModule

  def skipDeploy: Boolean = plugin("maven-deploy-plugin").flatMap(config)
    .flatMap(value(_, "skip")).fold(false)(_.toBoolean)

  def testForkArgs: Seq[Opt] = plugin("maven-surefire-plugin").flatMap(config)
    .flatMap(child(_, "systemPropertyVariables")).fold(Nil) { dom =>
      dom.getChildren.toSeq.map { dom =>
        val key = dom.getName
        val value = dom.getValue
        Opt(s"-D$key=$value")
      }
    }

  private def plugin(artifactId: String, groupId: String = "org.apache.maven.plugins") =
    model.getBuild.getPlugins.asScala.find(p =>
      p.getArtifactId == artifactId && p.getGroupId == groupId
    )

  private def config(cc: ConfigurationContainer) = cc.getConfiguration match {
    case dom: Xpp3Dom => Some(dom)
    case _ => None
  }

  private def child(dom: Xpp3Dom, name: String): Option[Xpp3Dom] =
    dom.getChild(name) match {
      case null => None
      case dom => Some(dom)
    }

  private def children(dom: Xpp3Dom, name: String): Seq[Xpp3Dom] =
    dom.getChildren(name).toSeq

  private def value(dom: Xpp3Dom, name: String): Option[String] =
    dom.getChild(name) match {
      case null => None
      case dom => Some(dom.getValue)
    }

  private def values(dom: Xpp3Dom, name: String): Seq[String] =
    dom.getChildren(name).toSeq.map(_.getValue)
}
