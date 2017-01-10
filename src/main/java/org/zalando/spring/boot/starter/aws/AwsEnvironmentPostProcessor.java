package org.zalando.spring.boot.starter.aws;

import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.context.config.ConfigFileApplicationListener;
import org.springframework.boot.env.EnvironmentPostProcessor;
import org.springframework.core.Ordered;
import org.springframework.core.env.CommandLinePropertySource;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.env.MutablePropertySources;
import org.springframework.core.env.PropertiesPropertySource;

import com.amazonaws.util.EC2MetadataUtils;

/**
 * A simple {@link EnvironmentPostProcessor} for a Spring Boot application that will
 * recognize when the application is running on AWS EC2 utilizing {@link EC2MetadataUtils} and will
 * expose various AWS properties on the environment.
 */
public class AwsEnvironmentPostProcessor implements EnvironmentPostProcessor, Ordered {

	private final Logger log = LoggerFactory.getLogger(AwsEnvironmentPostProcessor.class);

	/**
	 * The base prefix for all exposed properties
	 */
	public static final String AWS_PREFIX = "aws";

	/**
	 * The key for the enabled property
	 */
	public static final String AWS_ENABLED_KEY = AWS_PREFIX + ".enabled";

	/**
	 * The key for the AWS availability zone property
	 */
	public static final String AWS_AZ_KEY = AWS_PREFIX + ".az";

	/**
	 * The key for the AWS region property
	 */
	public static final String AWS_REGION_KEY = AWS_PREFIX + ".region";

	/**
	 * The key for the AWS hostname property
	 */
	public static final String AWS_HOSTNAME_KEY = AWS_PREFIX + ".localhostname";

	/**
	 * The key for the AWS local IPv4 address property
	 */
	public static final String AWS_LOCAL_IPV4_KEY = AWS_PREFIX + ".localipv4";

	/**
	 * The key for the AWS instance ID property
	 */
	public static final String AWS_INSTANCE_ID_KEY = AWS_PREFIX + ".instanceid";

	// Before ConfigFileApplicationListener so values there can use these ones
	private int order = ConfigFileApplicationListener.DEFAULT_ORDER - 1;

	/**
	 * Set the order for determining when the {@link EnvironmentPostProcessor} should be executed. If
	 * not set the order will default to {@link ConfigFileApplicationListener} DEFAULT_ORDER - 1.
	 *
	 * @param order  the int value for the order to set. See {@link Ordered} for more details.
	 */
	public void setOrder(int order) {
		this.order = order;
	}

	@Override
	public int getOrder() {
		return order;
	}

	@Override
	public void postProcessEnvironment(ConfigurableEnvironment environment, SpringApplication application) {
		log.info("Detecting 'AWS'-Environment ...");
		Properties properties = new Properties();
		if (awsMetadataServiceIsAvailable()) {
			log.info("Collect 'AWS'-metadata ...");
			properties.put(AWS_ENABLED_KEY, Boolean.TRUE.toString());
			properties.put(AWS_AZ_KEY, getAwsAvailabilityZone());
			properties.put(AWS_REGION_KEY, getAwsRegion());
			properties.put(AWS_HOSTNAME_KEY, getAwsLocalHostname());
			properties.put(AWS_LOCAL_IPV4_KEY, getAwsLocalIpV4());
			properties.put(AWS_INSTANCE_ID_KEY, getInstanceId());
			log.info("'AWS'-metadata : {}", properties.toString());
		} else {
			log.info("Ignore 'AWS', no metadata available.");
			properties.put(AWS_ENABLED_KEY, Boolean.FALSE.toString());
		}

		//
		MutablePropertySources propertySources = environment.getPropertySources();
		if (propertySources.contains(CommandLinePropertySource.COMMAND_LINE_PROPERTY_SOURCE_NAME)) {
			propertySources.addAfter(CommandLinePropertySource.COMMAND_LINE_PROPERTY_SOURCE_NAME,
					new PropertiesPropertySource("aws", properties));
		} else {
			propertySources.addFirst(new PropertiesPropertySource("aws", properties));
		}
	}

	/**
	 * Get the local IPv4 address.
	 *
	 * @return  the local IPv4 address string if available, null otherwise.
	 */
	private String getAwsLocalIpV4() {
		try {
			return EC2MetadataUtils.getNetworkInterfaces().get(0).getLocalIPv4s().get(0);
		} catch (Exception e) {
			return null;
		}
	}

	/**
	 * Get the availability zone.
	 *
	 * @return  the availability zone string.
	 */
	protected String getAwsAvailabilityZone() {
		return EC2MetadataUtils.getAvailabilityZone();
	}

	/**
	 * Get the region.
	 *
	 * @return  the region string.
	 */
	protected String getAwsRegion() {
		return EC2MetadataUtils.getEC2InstanceRegion();
	}

	/**
	 * Get the local hostname.
	 *
	 * @return  the local hostname string.
	 */
	protected String getAwsLocalHostname() {
		return EC2MetadataUtils.getLocalHostName();
	}

	/**
	 * Get the instance ID.
	 *
	 * @return  the instance ID string.
	 */
	protected String getInstanceId() {
		return EC2MetadataUtils.getInstanceId();
	}

	/**
	 * Get whether AWS metadata is available, i.e. whether the application is running on AWS.
	 *
	 * @return  true if AWS metadata is available, false otherwise.
	 */
	protected boolean awsMetadataServiceIsAvailable() {
		return EC2MetadataUtils.getInstanceId() != null;
	}
}
