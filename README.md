# Spring Boot Starter - AWS Support
This minimal Spring-Boot-Starter is a very simple `EnvironmentPostProcessor`
that detects whether an application is running on AWS or not. It utilizes the
`EC2MetadataUtils` to fetch some details about the AWS environment it's running on.

[![Build Status](https://travis-ci.org/zalando-incubator/aws-support-spring-boot-starter.svg?branch=master)](https://travis-ci.org/zalando-incubator/aws-support-spring-boot-starter)
[![Javadoc](https://javadoc-emblem.rhcloud.com/doc/org.zalando/aws-support-spring-boot-starter/badge.svg)](http://www.javadoc.io/doc/org.zalando/aws-support-spring-boot-starter)
[![Release](https://img.shields.io/github/release/zalando-incubator/aws-support-spring-boot-starter.svg)](https://github.com/zalando-incubator/aws-support-spring-boot-starter/releases)
[![Maven Central](https://img.shields.io/maven-central/v/org.zalando/aws-support-spring-boot-starter.svg)](https://maven-badges.herokuapp.com/maven-central/org.zalando/aws-support-spring-boot-starter)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/zalando-incubator/aws-support-spring-boot-starter/master/LICENSE)


## Usage
Simply add the JAR as a dependency to your project. It is available via Maven Central.

### Maven
Add the following to your `pom.xml`

```
<dependency>
    <groupId>org.zalando</groupId>
    <artifactId>aws-support-spring-boot-starter</artifactId>
    <version>${aws-support-spring-boot-starter.version}</version>
</dependency>
```

### Gradle
Add the following to your `build.gradle`

```
compile('org.zalando:aws-support-spring-boot-starter:$aws-support-spring-boot-starter.version')
```

### Download
You can download the `.jar` artifacts directly from the [Releases](https://github.com/zalando-incubator/aws-support-spring-boot-starter/releases) page here on GitHub.


### Exposed Properties
If the application is starting on AWS it will provide/set the following properties.

| Property          | Description       |
| ----------------- | ----------------- |
| aws.az            | Availability-Zone |
| aws.region        | Region            |
| aws.localhostname | Hostname          |
| aws.localipv4     | IP ( v. 4)        |
| aws.instanceid    | Instance-Id       |


## Contributing

### Issues
Found a bug or have an idea for a feature? You can help us by [creating an issue](https://github.com/zalando-incubator/aws-support-spring-boot-starter/issues).
Just make sure that an issue describing the bug or feature does not already exist. You can go even further and
[open a Pull Request](#pull-requests) with the fix or proposed feature.

### Pull Requests
Follow these steps to contribute your work to *Spring Boot Starter - AWS Support*:

1. [Open an issue](#issues) describing the problem or proposed feature. Assign yourself so we know you're working on it.
1. Fork this repo and create a branch for your work.
1. Push changes to your branch.
1. Test your changes.
1. Open a [Pull Request](https://github.com/zalando-incubator/aws-support-spring-boot-starter/pulls) when your code is ready for review.
    1. Mention the issue number in the comment (e.g. Fixes #37).
    1. If you're still working on it, add the **under development** label.
1. We will review your PR, give feedback, and merge when it is ready.

Thanks for your contribution!


## TODO
There are no further features planned at the moment. But we are open for any suggestions that should be added.


## Contact
Simply open an issues and assign the **question** label or reach out to any of the [maintainers](MAINTAINERS).


## Licence
The MIT License (MIT) Copyright © 2016 Zalando SE, https://tech.zalando.com

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
